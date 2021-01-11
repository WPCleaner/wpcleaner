/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import org.wikipediacleaner.api.API;
import org.wikipediacleaner.api.APIException;
import org.wikipediacleaner.api.APIFactory;
import org.wikipediacleaner.api.algorithm.AlgorithmParameter;
import org.wikipediacleaner.api.algorithm.AlgorithmParameterElement;
import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.configuration.WPCConfiguration;
import org.wikipediacleaner.api.configuration.WPCConfigurationStringList;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.DataManager;
import org.wikipediacleaner.api.data.Namespace;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.data.PageElementISBN;
import org.wikipediacleaner.api.data.Page.RelatedPages;
import org.wikipediacleaner.api.data.analysis.PageAnalysis;
import org.wikipediacleaner.api.data.contents.tag.HtmlTagType;
import org.wikipediacleaner.api.data.contents.template.TemplateBuilder;
import org.wikipediacleaner.i18n.GT;


/**
 * Algorithm for analyzing error 529 of check wikipedia project.
 * Error 529: ISBN magical link
 */
public class CheckErrorAlgorithm529 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm529() {
    super("ISBN magical link");
  }

  /** List of string that could be before an ISBN. */
  private final static String[] EXTEND_BEFORE_ISBN = {
    HtmlTagType.SMALL.getOpenTag(),
    "(",
  };

  /** List of string that could be after an ISBN. */
  private final static String[] EXTEND_AFTER_ISBN = {
    HtmlTagType.SMALL.getCloseTag(),
    ")",
  };

  /** Tracking category. */
  private String trackingCategory;

  /**
   * Analyze a page to check if errors are present.
   * 
   * @param analysis Page analysis.
   * @param errors Errors found in the page.
   * @param onlyAutomatic True if analysis could be restricted to errors automatically fixed.
   * @return Flag indicating if the error was found.
   */
  @Override
  public boolean analyze(
      PageAnalysis analysis,
      Collection<CheckErrorResult> errors, boolean onlyAutomatic) {
    if ((analysis == null) || (analysis.getPage() == null)) {
      return false;
    }

    // Analyze each ISBN
    boolean result = false;
    List<PageElementISBN> isbns = analysis.getISBNs();
    if ((isbns == null) || (isbns.isEmpty())) {
      return false;
    }
    String contents = analysis.getContents();
    for (PageElementISBN isbn : isbns) {
      boolean isError = false;
      if (!isbn.isTemplateParameter() && isbn.isCorrect()) {
        if (analysis.isInExternalLink(isbn.getBeginIndex()) == null) {
          isError = true;
        }
      }

      if (isError) {
        if (errors == null) {
          return true;
        }
        result = true;

        // Initialize
        int isbnBeginIndex = isbn.getBeginIndex();
        int isbnEndIndex = isbn.getEndIndex();
        boolean reported = false;

        // Try automatic replacements
        if (!automaticReplacements.isEmpty() && !isbnTemplates.isEmpty()) {
          for (String[] automaticReplacement : automaticReplacements) {
            if ((automaticReplacement != null) &&
                (automaticReplacement.length > 2) &&
                !reported) {
              String template = automaticReplacement[0];
              String prefix = automaticReplacement[1];
              String suffix = automaticReplacement[2];
              if ((isbnBeginIndex >= prefix.length()) &&
                  (contents.startsWith(prefix, isbnBeginIndex - prefix.length())) &&
                  (contents.startsWith(suffix, isbnEndIndex))) {
                CheckErrorResult errorResult = createCheckErrorResult(
                    analysis,
                    isbnBeginIndex - prefix.length(),
                    isbnEndIndex + suffix.length());
                for (String[] isbnTemplate : isbnTemplates) {
                  if ((isbnTemplate != null) &&
                      (isbnTemplate.length > 0) &&
                      (template.equals(isbnTemplate[0]))) {
                    addTemplateReplacement(errorResult, isbnTemplate, isbn, null, null, true);
                  }
                }
                errors.add(errorResult);
                reported = true;
              }
            }
          }
        }

        if (!reported) {
          // Try to extend area
          int beginIndex = isbnBeginIndex;
          boolean extensionFound = false;
          do {
            extensionFound = false;
            for (String before : EXTEND_BEFORE_ISBN) {
              if ((beginIndex >= before.length()) &&
                  (contents.startsWith(before, beginIndex - before.length()))) {
                extensionFound = true;
                beginIndex -= before.length();
              }
            }
          } while (extensionFound);
          int endIndex = isbnEndIndex;
          do {
            extensionFound = false;
            for (String after : EXTEND_AFTER_ISBN) {
              if ((endIndex < contents.length()) &&
                  (contents.startsWith(after, endIndex))) {
                extensionFound = true;
                endIndex += after.length();
              }
            }
          } while (extensionFound);
          String prefix = contents.substring(beginIndex, isbnBeginIndex);
          String suffix = contents.substring(isbnEndIndex, endIndex);
  
          CheckErrorResult errorResult = createCheckErrorResult(
              analysis, beginIndex, endIndex);
  
          // Suggest replacement with templates
          if (isbnTemplates != null) {
            for (String[] isbnTemplate : isbnTemplates) {
              addTemplateReplacement(errorResult, isbnTemplate, isbn, prefix, suffix, false);
            }
          }
  
          // Suggest replacement with interwikis
          for (String[] isbnInterwiki : isbnInterwikis) {
            if (isbnInterwiki.length > 0) {
              String isbnCode = isbnInterwiki[0];
              StringBuilder replacement = new StringBuilder();
              replacement.append("[[:");
              replacement.append(isbnCode);
              replacement.append(":");
              replacement.append(isbn.getISBN());
              replacement.append("|");
              replacement.append(PageElementISBN.ISBN_PREFIX);
              replacement.append(" ");
              replacement.append(isbn.getISBN());
              replacement.append("]]");
              errorResult.addReplacement(replacement.toString());
            }
          }
  
          errors.add(errorResult);
        }
      }
    }

    return result;
  }

  /**
   * Add a suggestion for replacement with a template.
   * 
   * @param errorResult Error result.
   * @param isbnTemplate Definition of the template.
   * @param isbn ISBN.
   * @param prefix Optional prefix.
   * @param suffix Optional suffix.
   * @param automatic True if automatic replacement can be performed.
   */
  private void addTemplateReplacement(
      CheckErrorResult errorResult, String[] isbnTemplate,
      PageElementISBN isbn, String prefix, String suffix,
      boolean automatic) {
    if ((isbnTemplate == null) || (isbnTemplate.length <= 2)) {
      return;
    }
    String templateName = isbnTemplate[0];
    String[] params = isbnTemplate[1].split(",");
    Boolean suggested = Boolean.valueOf(isbnTemplate[2]);
    if ((params.length > 0) && (Boolean.TRUE.equals(suggested))) {
      TemplateBuilder builder = TemplateBuilder.from(templateName);
      builder.addParam(
          !"1".equals(params[0]) ? params[0] : null,
          isbn.getISBNNotTrimmed());
      errorResult.addReplacement(builder.toString(), automatic);
      if (((prefix != null) && !prefix.isEmpty()) ||
          ((suffix != null) && !suffix.isEmpty())) {
        errorResult.addReplacement(
            (prefix != null ? prefix : "") + builder.toString() + (suffix != null ? suffix : ""));
      }
    }
  }

  /**
   * @return True if the error has a special list of pages.
   */
  @Override
  public boolean hasSpecialList() {
    return (getTrackingCategory() != null);
  }

  /**
   * @param category Tracking category.
   */
  public void setTrackingCategory(String category) {
    trackingCategory = category;
  }

  /**
   * @return Tracking category.
   */
  private String getTrackingCategory() {
    if (categoryName != null) {
      return categoryName;
    }
    if ((trackingCategory != null) &&
        (trackingCategory.trim().length() > 0)) {
      return trackingCategory;
    }
    return null;
  }

  /**
   * Retrieve the list of pages in error.
   * 
   * @param wiki Wiki.
   * @param limit Maximum number of pages to retrieve.
   * @return List of pages in error.
   */
  @Override
  public List<Page> getSpecialList(EnumWikipedia wiki, int limit) {
    List<Page> result = null;
    String category = getTrackingCategory();
    if (category != null) {
      API api = APIFactory.getAPI();
      String title = wiki.getWikiConfiguration().getPageTitle(Namespace.CATEGORY, category);
      Page categoryPage = DataManager.getPage(wiki, title, null, null, null);
      try {
        api.retrieveCategoryMembers(wiki, categoryPage, 0, false, limit);
        result = categoryPage.getRelatedPages(RelatedPages.CATEGORY_MEMBERS);
      } catch (APIException e) {
        //
      }
    }
    return result;
  }

  /**
   * Automatic fixing of some errors in the page.
   * 
   * @param analysis Page analysis.
   * @return Page contents after fix.
   */
  @Override
  protected String internalAutomaticFix(PageAnalysis analysis) {
    return fixUsingAutomaticReplacement(analysis);
  }

  /* ====================================================================== */
  /* PARAMETERS                                                             */
  /* ====================================================================== */

  /** Automatic replacements */
  private static final String PARAMETER_AUTOMATIC = "automatic";

  /** Category containing the list of pages in error */
  private static final String PARAMETER_CATEGORY = "category";

  /**
   * Initialize settings for the algorithm.
   * 
   * @see org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithmBase#initializeSettings()
   */
  @Override
  protected void initializeSettings() {
    String tmp = getSpecificProperty(PARAMETER_CATEGORY, true, true, false);
    categoryName = null;
    if ((tmp != null) &&
        (tmp.trim().length() > 0)) {
      categoryName = tmp.trim();
    }

    tmp = getSpecificProperty(PARAMETER_AUTOMATIC, true, true, true);
    automaticReplacements.clear();
    if (tmp != null) {
      List<String[]> tmpList = WPCConfiguration.convertPropertyToStringArrayList(tmp);
      if (tmpList != null) {
        for (String[] tmpItem : tmpList) {
          if (tmpItem.length > 2) {
            automaticReplacements.add(tmpItem);
          }
        }
      }
    }

    List<String[]> tmpList = getWPCConfiguration().getStringArrayList(
        WPCConfigurationStringList.ISBN_TEMPLATES);
    isbnTemplates.clear();
    if (tmpList != null) {
      for (String[] isbnTemplate : tmpList) {
        if (isbnTemplate.length > 2) {
          isbnTemplates.add(isbnTemplate);
        }
      }
    }

    tmpList = getWPCConfiguration().getStringArrayList(
        WPCConfigurationStringList.ISBN_INTERWIKIS);
    isbnInterwikis.clear();
    if (tmpList != null) {
      for (String[] isbnInterwiki : tmpList) {
        if (isbnInterwiki.length > 0) {
          isbnInterwikis.add(isbnInterwiki);
        }
      }
    }
  }

  /** Category containing the list of pages in error */
  private String categoryName = null;

  /** Templates for ISBN */
  private List<String[]> isbnTemplates = new ArrayList<>();

  /** Interwikis for ISBN */
  private List<String[]> isbnInterwikis = new ArrayList<>();

  /** Automatic replacements for ISBN */
  private List<String[]> automaticReplacements = new ArrayList<>();

  /**
   * Build the list of parameters for this algorithm.
   */
  @Override
  protected void addParameters() {
    super.addParameters();
    addParameter(new AlgorithmParameter(
        PARAMETER_AUTOMATIC,
        GT._T("Automatic replacements of ISBN"),
        new AlgorithmParameterElement[] {
            new AlgorithmParameterElement(
                "template name",
                GT._T("Name of the template to use for ISBN")),
            new AlgorithmParameterElement(
                "prefix",
                GT._T("Text before the ISBN to be removed in the replacement")),
            new AlgorithmParameterElement(
                "suffix",
                GT._T("Text after the ISBN to be removed in the replacement"))
        },
        true));
    addParameter(new AlgorithmParameter(
        PARAMETER_CATEGORY,
        GT._T("A category containing the list of pages in error"),
        new AlgorithmParameterElement(
            "category name",
            GT._T("A category containing the list of pages in error"))));
  }
}

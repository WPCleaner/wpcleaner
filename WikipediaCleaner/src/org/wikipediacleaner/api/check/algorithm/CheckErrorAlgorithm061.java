/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;

import org.wikipediacleaner.api.algorithm.AlgorithmParameter;
import org.wikipediacleaner.api.algorithm.AlgorithmParameterElement;
import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.check.SpecialCharacters;
import org.wikipediacleaner.api.constants.WPCConfiguration;
import org.wikipediacleaner.api.data.PageElement;
import org.wikipediacleaner.api.data.PageElementFullTag;
import org.wikipediacleaner.api.data.PageElementTag;
import org.wikipediacleaner.api.data.PageElementTemplate;
import org.wikipediacleaner.api.data.analysis.PageAnalysis;
import org.wikipediacleaner.api.data.contents.IntervalComparator;
import org.wikipediacleaner.i18n.GT;


/**
 * Algorithm for analyzing error 61 of check wikipedia project.
 * Error 61: Reference with punctuation
 */
public class CheckErrorAlgorithm061 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm061() {
    super("Reference before punctuation");
  }

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
    if (analysis == null) {
      return false;
    }

    // Analyze from the beginning
    List<PageElement> refs = getRefs(analysis);
    if ((refs == null) || (refs.isEmpty())) {
      return false;
    }
    boolean result = false;
    String contents = analysis.getContents();
    int refIndex = 0;
    int maxRefs = refs.size();
    while (refIndex < maxRefs) {

      // Group references separated only by punctuation characters
      int firstRefIndex = refIndex;
      PageElement firstRef = refs.get(firstRefIndex);
      int lastRefIndex = PageElement.groupElements(refs, firstRefIndex, contents, ",;.\'", separator);
      PageElement lastRef = refs.get(lastRefIndex);
      refIndex = lastRefIndex + 1;

      // Remove possible whitespace characters after last reference
      int tmpIndex = lastRef.getEndIndex();
      boolean finished = false;
      while (!finished) {
        if (tmpIndex >= contents.length()) {
          finished = true;
        } else if (contents.charAt(tmpIndex) == '\n') {
          if ((tmpIndex + 1 < contents.length()) &&
              ((Character.isWhitespace(contents.charAt(tmpIndex + 1))) ||
               (contents.charAt(tmpIndex + 1) == '*') || // List
               (contents.charAt(tmpIndex + 1) == '#') || // List
               (contents.charAt(tmpIndex + 1) == ';') || // Definition
               (contents.charAt(tmpIndex + 1) == ':') || // Definition
               (contents.charAt(tmpIndex + 1) == '!'))) { // Table heading
            finished = true;
          } else {
            tmpIndex++;
          }
        } else if (Character.isWhitespace(contents.charAt(tmpIndex))) {
          tmpIndex++;
        } else {
          finished = true;
        }
      }

      // Check if next character is a punctuation
      int firstPunctuationIndex = -1;
      char punctuation = ' ';
      if (tmpIndex < contents.length()) {
        punctuation = contents.charAt(tmpIndex);
        if (SpecialCharacters.isPunctuation(punctuation)) {
          // TODO: Once tables are managed by parser, remove the this trick that prevent detection before "!!"
          if ((punctuation != '!') ||
              (tmpIndex + 1 >= contents.length()) ||
              (contents.charAt(tmpIndex + 1) != punctuation)) {
            firstPunctuationIndex = tmpIndex;

            // Check if the punctuation after is multiple
            while ((tmpIndex < contents.length()) &&
                   (contents.charAt(tmpIndex) == punctuation)) {
              tmpIndex++;
            }
          }
        }
      }

      // Check if error should be reported
      boolean report = false;
      if (firstPunctuationIndex >= 0) {
        if (tmpIndex >= contents.length()) {
          report = true;
        } else {
          char nextChar = contents.charAt(tmpIndex);
          if (Character.isWhitespace(nextChar)) {
            report = true;
          }
        }
      }

      // Error found
      if (report) {
        if (errors == null) {
          return true;
        }
        result = true;

        int beginIndex = firstRef.getBeginIndex();
        int endIndex = tmpIndex;
        String allPunctuations = contents.substring(firstPunctuationIndex, endIndex);

        // Construct list of tags
        String replace = PageElement.createListOfElements(
            refs, firstRefIndex, lastRefIndex, contents, separator);
        String textReplace = createReducedListOfRefs(
            lastRefIndex - firstRefIndex + 1, separator);

        // Check for possible punctuation before tags
        tmpIndex = beginIndex - 1;
        while ((tmpIndex >= 0) &&
               (contents.charAt(tmpIndex) == ' ')) {
          tmpIndex--;
        }
        beginIndex = tmpIndex + 1;
        boolean punctuationFoundBefore = false;
        int punctuationBeforeIndex = tmpIndex;
        while ((tmpIndex >= 0) &&
               SpecialCharacters.isPunctuation(contents.charAt(tmpIndex))) {
          punctuationFoundBefore = true;
          tmpIndex--;
        }
        String punctuationBefore = contents.substring(tmpIndex + 1, punctuationBeforeIndex + 1);
        if (punctuationFoundBefore) {
          beginIndex = tmpIndex + 1;
        }

        // Create error
        CheckErrorResult errorResult = createCheckErrorResult(
            analysis, beginIndex, endIndex);
        errorResult.addReplacement(
            allPunctuations + replace,
            allPunctuations + textReplace);
        if (punctuationFoundBefore &&
            !allPunctuations.equals(punctuationBefore)) {
          errorResult.addReplacement(
              punctuationBefore + replace,
              punctuationBefore + textReplace);
        }
        errors.add(errorResult);
      }
    }
    return result;
  }

  /**
   * @param analysis Page analysis.
   * @return List of references (tags, templates, ...).
   */
  private List<PageElement> getRefs(PageAnalysis analysis) {
    List<PageElement> refs = new ArrayList<PageElement>();

    // Retrieve references defined by tags
    List<PageElementTag> refTags = analysis.getCompleteTags(PageElementTag.TAG_WIKI_REF);
    if (refTags != null) {
      for (PageElementTag refTag : refTags) {
        refs.add(new PageElementFullTag(refTag));
      }
    }

    // Retrieve references defined by templates
    for (String templateName : templatesName) {
      List<PageElementTemplate> templates = analysis.getTemplates(templateName.trim());
      if (templates != null) {
        refs.addAll(templates);
      }
    }

    Collections.sort(refs, new IntervalComparator());
    return refs;
  }

  /**
   * Create a reduced textual representation of a list of references.
   * 
   * @param count Number of references in the list.
   * @param separator Separator.
   * @return Reduced textual representation of a list of references.
   */
  public static String createReducedListOfRefs(
      int count, String separator) {
    if (count > 2) {
      return "<ref>...</ref>" + separator + "..." + separator + "<ref>...</ref>";
    }
    if (count > 1) {
      return "<ref>...</ref>" + separator + "<ref>...</ref>";
    }
    return "<ref>...</ref>";
  }

  /* ====================================================================== */
  /* PARAMETERS                                                             */
  /* ====================================================================== */

  /** Separator between consecutive tags */
  private static final String PARAMETER_SEPARATOR = "separator";

  /** Templates that can replace a tag */
  private static final String PARAMETER_TEMPLATES = "templates";

  /**
   * Initialize settings for the algorithm.
   * 
   * @see org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithmBase#initializeSettings()
   */
  @Override
  protected void initializeSettings() {
    separator = getSpecificProperty(PARAMETER_SEPARATOR, true, false, false);
    if (separator == null) {
      separator = "";
    }

    String tmp = getSpecificProperty(PARAMETER_TEMPLATES, true, true, false);
    templatesName.clear();
    if (tmp != null) {
      List<String> tmpList = WPCConfiguration.convertPropertyToStringList(tmp);
      if (tmpList != null) {
        templatesName.addAll(tmpList);
      }
    }
  }

  /** Separator between consecutive tags */
  private String separator = "";

  /** Templates that can replace a tag */
  private final List<String> templatesName = new ArrayList<>();

  /**
   * Build the list of parameters for this algorithm.
   */
  @Override
  protected void addParameters() {
    super.addParameters();
    addParameter(new AlgorithmParameter(
        PARAMETER_SEPARATOR,
        GT._T("Used as a separator between consecutive {0} tags", "&lt;ref&gt;"),
        new AlgorithmParameterElement(
            "text",
            GT._T("Used as a separator between consecutive {0} tags", "&lt;ref&gt;"))));
    addParameter(new AlgorithmParameter(
        PARAMETER_TEMPLATES,
        GT._T("Templates that can be used to replace {0} tags", "&lt;ref&gt;"),
        new AlgorithmParameterElement(
            "template name",
            GT._T("Template that can be used to replace {0} tags", "&lt;ref&gt;")),
        true));
  }
}

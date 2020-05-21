/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.regex.PatternSyntaxException;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.wikipediacleaner.api.API;
import org.wikipediacleaner.api.APIException;
import org.wikipediacleaner.api.APIFactory;
import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.check.SimpleAction;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.constants.WPCConfiguration;
import org.wikipediacleaner.api.data.CharacterUtils;
import org.wikipediacleaner.api.data.LinterCategory;
import org.wikipediacleaner.api.data.Namespace;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.data.PageAnalysis;
import org.wikipediacleaner.api.data.PageElementExternalLink;
import org.wikipediacleaner.api.data.PageElementInternalLink;
import org.wikipediacleaner.api.data.PageElementTemplate;
import org.wikipediacleaner.api.data.contents.ContentsElement;
import org.wikipediacleaner.gui.swing.action.ActionExternalViewer;
import org.wikipediacleaner.i18n.GT;


/**
 * Algorithm for analyzing error 511 of check wikipedia project.
 * Error 513: Internal link inside external link
 */
public class CheckErrorAlgorithm513 extends CheckErrorAlgorithmBase {

  /** Logs */
  private final Logger log = LoggerFactory.getLogger(CheckErrorAlgorithm513.class);

  public CheckErrorAlgorithm513() {
    super("Internal link inside external link");
  }

  private static final String PUNCTUATION = ",-:(";

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

    // Analyze each external link
    boolean result = false;
    List<PageElementExternalLink> links = analysis.getExternalLinks();
    List<PageElementInternalLink> iLinks = analysis.getInternalLinks();
    if ((links != null) && !links.isEmpty() &&
        (iLinks != null) && !iLinks.isEmpty()) {
      for (PageElementExternalLink link : links) {
        result |= analyzeExternalLink(link, analysis, errors);
      }
    }

    // Analyze each template
    List<PageElementTemplate> articleTemplates = analysis.getTemplates();
    if ((articleTemplates != null) &&
        ((iLinks != null) && !iLinks.isEmpty())) {
      for (PageElementTemplate template : articleTemplates) {
        result |= analyzeTemplate(template, analysis, errors);
      }
    }
    return result;
  }

  /**
   * Analyze one template.
   * 
   * @param template Template to analyze.
   * @param analysis Page analysis.
   * @param errors Errors found in the page.
   * @return Flag indicating if an error was found in the external link.
   */
  private boolean analyzeTemplate(
      PageElementTemplate template,
      PageAnalysis analysis,
      Collection<CheckErrorResult> errors) {

    // Check if there's a configuration for the template
    List<String[]> paramsConfig = templateParams.get(template.getTemplateName());
    if ((paramsConfig == null) || paramsConfig.isEmpty()) {
      return false;
    }

    // Check each configuration
    boolean result = false;
    for (String[] paramConfig : paramsConfig) {
      int paramIndex = template.getParameterIndex(paramConfig[1]);
      if (paramIndex >= 0) {
        PageElementTemplate.Parameter param = template.getParameter(paramIndex);
        if (param != null) {
          String contents = analysis.getContents();
          int squareBracket = contents.indexOf("[[", param.getBeginIndex());
          boolean reported = false;
          while (!reported && (squareBracket > 0) &&
                 (squareBracket < param.getEndIndex())) {
            PageElementInternalLink link = analysis.isInInternalLink(squareBracket);
            if (link != null) {
              if (errors == null) {
                return true;
              }
              reported = true;
              result = true;
              CheckErrorResult errorResult = createCheckErrorResult(
                  analysis, param.getBeginIndex(), param.getEndIndex());
              String replacement =
                  contents.substring(param.getBeginIndex(), link.getBeginIndex()) +
                  link.getDisplayedTextNotTrimmed() +
                  contents.substring(link.getEndIndex(), param.getEndIndex());
              errorResult.addReplacement(replacement);
              errors.add(errorResult);
            }
            squareBracket++;
          }
        }
      }
    }

    return result;
  }

  /**
   * Analyze one external link.
   * 
   * @param link External link to analyze.
   * @param analysis Page analysis.
   * @param errors Errors found in the page
   * @return Flag indicating if an error was found in the external link.
   */
  private boolean analyzeExternalLink(
      PageElementExternalLink link,
      PageAnalysis analysis,
      Collection<CheckErrorResult> errors) {

    // Criteria on the external link itself
    if (!link.hasSquare()) {
      return false;
    }

    // Criteria on the internal link inside the external link
    ContentsElement internalLink = null;
    String internalLinkText = null;
    if (!link.hasSecondSquare()) {
      PageElementInternalLink tmp = analysis.isInInternalLink(link.getEndIndex());
      if ((tmp != null) &&
          (tmp.getBeginIndex() == link.getEndIndex())) {
        internalLink = tmp;
        internalLinkText = tmp.getDisplayedTextNotTrimmed();
      }
    }
    String contents = analysis.getContents();
    if ((internalLink == null) && !templates.isEmpty()) {
      int tmpIndex = link.getBeginIndex() + link.getTextOffset();
      while ((internalLink == null) && (tmpIndex < link.getEndIndex())) {
        if (contents.startsWith("{{", tmpIndex)) {
          PageElementTemplate template = analysis.isInTemplate(tmpIndex);
          if (template != null) {
            String[] config = templates.get(template.getTemplateName());
            if (config != null) {
              internalLink = template;
              if (config.length > 1) {
                if (errors == null) {
                  return true;
                }
                CheckErrorResult errorResult = createCheckErrorResult(
                    analysis, link.getBeginIndex(), link.getEndIndex());
                int endTemplateName = (template.getParameterCount() > 0) ?
                    template.getParameterPipeIndex(0) :
                    template.getEndIndex() - 2;
                String replacementEnd = 
                    "{{" + config[1] +
                    contents.substring(endTemplateName, link.getEndIndex());
                String replacement =
                    contents.substring(link.getBeginIndex(), template.getBeginIndex()) +
                    replacementEnd;
                String description =
                    "[..." +
                    contents.substring(link.getLinkEndIndex(), template.getBeginIndex()) +
                    replacementEnd;
                boolean automatic = (config.length > 2) ?
                    Boolean.valueOf(config[2]) : false;
                errorResult.addReplacement(replacement, description, automatic);
                errors.add(errorResult);
                return true;
              }
            }
          }
        }
        tmpIndex++;
      }
    }
    if (internalLink == null) {
      return false;
    }

    // Report error immediately if no list of errors
    if (errors == null) {
      return true;
    }

    // Analyze contents for potential replacements
    int beginExtra = internalLink.getBeginIndex();
    int endExtra = internalLink.getEndIndex();
    boolean automatic = false;
    if ((endExtra < contents.length()) &&
        contents.startsWith("''", endExtra)) {
      int countQuote = 2;
      while ((endExtra + countQuote < contents.length()) &&
          (contents.charAt(endExtra + countQuote) == '\'')) {
        countQuote++;
      }
      if ((countQuote == 2) ||
          (countQuote == 3) ||
          (countQuote == 5)) {
        if ((beginExtra > countQuote) &&
            contents.substring(0, beginExtra).endsWith(contents.substring(endExtra, endExtra + countQuote))) {
          beginExtra -= countQuote;
          endExtra += countQuote;
        }
      }
    }
    while ((beginExtra > 0) &&
        CharacterUtils.isWhitespace(contents.charAt(beginExtra - 1))) {
      beginExtra--;
    }
    boolean checkTexts = true;
    while (checkTexts) {
      String prefix = contents.substring(link.getBeginIndex(), beginExtra);
      checkTexts = false;
      for (String[] text : textsBefore) {
        if (prefix.endsWith(text[0])) {
          beginExtra -= text[0].length();
          automatic |= text.length > 1 && Boolean.parseBoolean(text[1]);
          checkTexts = true;
          continue;
        }
      }
      while ((beginExtra > 0) &&
          (CharacterUtils.isWhitespace(contents.charAt(beginExtra - 1)) ||
           (PUNCTUATION.indexOf(contents.charAt(beginExtra - 1)) >= 0))) {
        if (PUNCTUATION.indexOf(contents.charAt(beginExtra - 1)) >= 0) {
          automatic = true;
        }
        beginExtra--;
      }
    }
    if (beginExtra <= link.getBeginIndex() + link.getTextOffset()) {
      automatic = false;
    }
    checkTexts = true;
    while (checkTexts) {
      checkTexts = false;
      for (Pattern pattern : patternsAfter) {
        Matcher matcher = pattern.matcher(contents.substring(endExtra));
        if (matcher.lookingAt()) {
          endExtra += matcher.end();
          checkTexts = true;
        }
      }
    }

    // Check for extra bracket at the end
    boolean closeBracket = false;
    int beginError = link.getBeginIndex();
    int endError = endExtra;
    while ((endError < contents.length()) &&
        (CharacterUtils.isWhitespace(contents.charAt(endError)) ||
         (".)".indexOf(contents.charAt(endError)) >= 0))) {
      endError++;
    }
    int tmpEnd = endError;
    while ((tmpEnd < contents.length()) &&
        ("\n[]{}".indexOf(contents.charAt(tmpEnd)) < 0)) {
      tmpEnd++;
    }
    if ((tmpEnd < contents.length()) &&
        (tmpEnd > endError) &&
        (contents.charAt(tmpEnd) == ']')) {
      endError = tmpEnd;
      automatic = false;
    }
    if ((endError < contents.length()) &&
        (contents.charAt(endError) == ']')) {
      closeBracket = true;
      endError++;
    }

    // Report error
    CheckErrorResult errorResult = createCheckErrorResult(
        analysis, beginError, endError);
    if (closeBracket) {
      String replacementEnd =
          "]" +
          contents.substring(beginExtra, endError - 1);
      String replacement =
          contents.substring(beginError, beginExtra) +
          replacementEnd;
      String description = 
          "[..." +
          contents.substring(link.getLinkEndIndex(), beginExtra) +
          replacementEnd;
      errorResult.addReplacement(replacement, description, automatic);
    }
    if (internalLinkText != null) {
      String replacementEnd = 
          internalLinkText +
          contents.substring(internalLink.getEndIndex(), endError);
      String replacement =
          contents.substring(beginError, internalLink.getBeginIndex()) +
          replacementEnd;
      String description =
          "[..." +
          contents.substring(link.getLinkEndIndex(), internalLink.getBeginIndex()) +
          replacementEnd;
      errorResult.addReplacement(replacement, description);
    }
    errorResult.addPossibleAction(new SimpleAction(
        GT._T("External Viewer"),
        new ActionExternalViewer(link.getLink())));
    errors.add(errorResult);
    return true;
  }

  /**
   * Automatic fixing of all the errors in the page.
   * 
   * @param analysis Page analysis.
   * @return Page contents after fix.
   */
  @Override
  protected String internalAutomaticFix(PageAnalysis analysis) {
    if (!analysis.getPage().isArticle() ||
        !analysis.getPage().isInMainNamespace()) {
      return analysis.getContents();
    }
    return fixUsingAutomaticReplacement(analysis);
  }

  /**
   * @return True if the error has a special list of pages.
   */
  @Override
  public boolean hasSpecialList() {
    return (linterCategory != null);
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
    if (linterCategory != null) {
      API api = APIFactory.getAPI();
      try {
        result = api.retrieveLinterCategory(
            wiki, linterCategory.getCategory(),
            Namespace.MAIN, false, true, limit);
      } catch (APIException e) {
        //
      }
    }
    return result;
  }

  /* ====================================================================== */
  /* PARAMETERS                                                             */
  /* ====================================================================== */

  /** List of potential texts that can be used after the internal link */
  private static final String PARAMETER_TEXTS_AFTER = "texts_after";

  /** List of potential texts that can be used before the internal link */
  private static final String PARAMETER_TEXTS_BEFORE = "texts_before";

  /** List of templates that create an external link */
  private static final String PARAMETER_TEMPLATES = "templates";

  /** List of template parameters that create an external link */
  private static final String PARAMETER_TEMPLATE_PARAMS = "template_params";

  /**
   * Initialize settings for the algorithm.
   * 
   * @see org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithmBase#initializeSettings()
   */
  @Override
  protected void initializeSettings() {
    String tmp = getSpecificProperty(PARAMETER_TEXTS_AFTER, true, true, false);
    patternsAfter.clear();
    if (tmp != null) {
      List<String> tmpList = WPCConfiguration.convertPropertyToStringList(tmp);
      if (tmpList != null) {
        for (String element : tmpList) {
          try {
            patternsAfter.add(Pattern.compile(element));
          } catch (PatternSyntaxException e) {
            log.warn("Incorrect pattern in {} for error #518: {}", PARAMETER_TEXTS_AFTER, element);
          }
        }
      }
    }

    tmp = getSpecificProperty(PARAMETER_TEXTS_BEFORE, true, true, false);
    textsBefore.clear();
    if (tmp != null) {
      List<String[]> tmpList = WPCConfiguration.convertPropertyToStringArrayList(tmp);
      if (tmpList != null) {
        textsBefore.addAll(tmpList);
      }
    }

    tmp = getSpecificProperty(PARAMETER_TEMPLATES, true, true, false);
    templates.clear();
    if (tmp != null) {
      List<String[]> tmpList = WPCConfiguration.convertPropertyToStringArrayList(tmp);
      if (tmpList != null) {
        for (String[] tmpElement : tmpList) {
          templates.put(tmpElement[0], tmpElement);
        }
      }
    }

    tmp = getSpecificProperty(PARAMETER_TEMPLATE_PARAMS, true, true, false);
    templateParams.clear();
    if (tmp != null) {
      List<String[]> tmpList = WPCConfiguration.convertPropertyToStringArrayList(tmp);
      if (tmpList != null) {
        for (String[] tmpElement : tmpList) {
          if ((tmpElement != null) && (tmpElement.length > 1)) {
            List<String[]> values = templateParams.get(tmpElement[0]);
            if (values == null) {
              values = new ArrayList<>();
              templateParams.put(tmpElement[0], values);
            }
            values.add(tmpElement);
          }
        }
      }
    }

    List<LinterCategory> categories = getWikiConfiguration().getLinterCategories();
    if (categories != null) {
      for (LinterCategory category : categories) {
        if ("wikilink-in-extlink".equals(category.getCategory())) {
          linterCategory = category;
        }
      }
    }
  }

  /** Texts after the internal link */
  private final List<Pattern> patternsAfter = new ArrayList<>();

  /** Texts before the internal link */
  private final List<String[]> textsBefore = new ArrayList<>();

  /** Templates that create an external link */
  private final Map<String, String[]> templates = new HashMap<>();

  /** Template parameters that create an external link */
  private final Map<String, List<String[]>> templateParams = new HashMap<>();

  /** Linter category */
  private LinterCategory linterCategory = null;

  /**
   * @return Map of parameters (key=name, value=description).
   * @see org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithmBase#getParameters()
   */
  @Override
  public Map<String, String> getParameters() {
    Map<String, String> parameters = super.getParameters();
    parameters.put(
        PARAMETER_TEMPLATES,
        GT._T("A list of templates that create an internal link"));
    parameters.put(
        PARAMETER_TEMPLATE_PARAMS,
        GT._T("A list of template parameters that create the text of an external link"));
    parameters.put(
        PARAMETER_TEXTS_AFTER,
        GT._T("A list of texts (regular expressions) that can be after the internal link"));
    parameters.put(
        PARAMETER_TEXTS_BEFORE,
        GT._T("A list of texts that can be before the internal link"));
    return parameters;
  }
}

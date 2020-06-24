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
import org.wikipediacleaner.api.algorithm.AlgorithmParameter;
import org.wikipediacleaner.api.algorithm.AlgorithmParameterElement;
import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.check.SimpleAction;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.constants.WPCConfiguration;
import org.wikipediacleaner.api.data.CharacterUtils;
import org.wikipediacleaner.api.data.LinterCategory;
import org.wikipediacleaner.api.data.Namespace;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.data.PageElementExternalLink;
import org.wikipediacleaner.api.data.PageElementInternalLink;
import org.wikipediacleaner.api.data.PageElementTemplate;
import org.wikipediacleaner.api.data.PageRedirect;
import org.wikipediacleaner.api.data.analysis.PageAnalysis;
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

  /** Punctuation characters before the internal link that trigger automatic replacement */
  private static final String AUTOMATIC_PUNCTUATION_BEFORE = ",-–—:(";

  /** Punctuation characters before the internal link */
  private static final String PUNCTUATION_BEFORE = "" + AUTOMATIC_PUNCTUATION_BEFORE;

  /** Punctuation characters after the internal link */
  private static final String PUNCTUATION_AFTER = ",-–—:)";

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
    if ((links != null) && !links.isEmpty()) {
      boolean analyzeLinks = false;
      if ((iLinks != null) && !iLinks.isEmpty()) {
        analyzeLinks = true;
      }
      if (!templates.isEmpty()) {
        analyzeLinks = true;
      }
      if (analyzeLinks) {
        for (PageElementExternalLink link : links) {
          result |= analyzeExternalLink(link, analysis, errors);
        }
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
      result |= analyzeTemplateParam(template, paramConfig, analysis, errors);
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
  private boolean analyzeTemplateParam(
      PageElementTemplate template,
      String[] paramConfig,
      PageAnalysis analysis,
      Collection<CheckErrorResult> errors) {

    // Extract template parameter
    int paramIndex = template.getParameterIndex(paramConfig[1]);
    if (paramIndex < 0) {
      return false;
    }
    PageElementTemplate.Parameter param = template.getParameter(paramIndex);
    if (param == null) {
      return false;
    }

    // Analyze parameter content
    int tmpIndex = param.getBeginIndex();
    boolean closeBracket = false;
    String contents = analysis.getContents();
    while (tmpIndex < param.getEndIndex()) {
      if (contents.charAt(tmpIndex) == ']') {
        closeBracket = true;
        // TODO: Stop analyzing or depending on template configuration?
        return false;
      } else if (contents.startsWith("[[", tmpIndex)) {

        // Report internal link
        PageElementInternalLink link = analysis.isInInternalLink(tmpIndex);
        if ((link != null) && (link.getBeginIndex() == tmpIndex)) {
          if (errors == null) {
            return true;
          }
          CheckErrorResult errorResult = createCheckErrorResult(
              analysis, param.getBeginIndex(), param.getEndIndex());
          String replacement =
              contents.substring(param.getBeginIndex(), link.getBeginIndex()) +
              link.getDisplayedTextNotTrimmed() +
              contents.substring(link.getEndIndex(), param.getEndIndex());
          boolean automatic = (paramConfig.length > 2) ?
              Boolean.valueOf(paramConfig[2]) : false;
          automatic &= (template == analysis.isInTemplate(tmpIndex));
          errorResult.addReplacement(replacement, automatic && !closeBracket);
          errors.add(errorResult);
          return true;
        }
      } else if (contents.startsWith("{{", tmpIndex)) {

        // Report template
        PageElementTemplate tmpTemplate = analysis.isInTemplate(tmpIndex);
        if ((tmpTemplate != null) &&
            (tmpTemplate.getBeginIndex() == tmpIndex)) {
          TemplateReplacement config = templates.get(tmpTemplate.getTemplateName());
          if (config != null) {
            if (errors == null) {
              return true;
            }
            CheckErrorResult errorResult = createCheckErrorResult(
                analysis, param.getBeginIndex(), param.getEndIndex());
            if (config.replacement != null) {
              String replacementEnd = null;
              if (config.fullReplacement) {
                replacementEnd =
                    config.replacement +
                    contents.substring(tmpTemplate.getEndIndex(), param.getEndIndex());
              } else {
                int endTemplateName = (tmpTemplate.getParameterCount() > 0) ?
                    tmpTemplate.getParameterPipeIndex(0) :
                    tmpTemplate.getEndIndex() - 2;
                replacementEnd = 
                    "{{" + config.replacement +
                    contents.substring(endTemplateName, param.getEndIndex());
              }
              String replacement =
                  contents.substring(param.getBeginIndex(), tmpTemplate.getBeginIndex()) +
                  replacementEnd;
              errorResult.addReplacement(replacement, config.automatic && !closeBracket);
            }
            errors.add(errorResult);
            return true;
          }
        }
      }
      tmpIndex++;
    }

    return false;
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
    boolean internalLinkFirst = false;
    boolean internalLinkLast = false;
    boolean internalLinkFull = false;
    if (!link.hasSecondSquare()) {
      PageElementInternalLink tmp = analysis.isInInternalLink(link.getEndIndex());
      if ((tmp != null) &&
          (tmp.getBeginIndex() == link.getEndIndex())) {
        internalLink = tmp;
        internalLinkText = tmp.getDisplayedTextNotTrimmed();
        internalLinkFirst = isInList(tmp, linksFirst, analysis, link);
        internalLinkLast = isInList(tmp, linksLast, analysis, link);
        internalLinkFull = isInList(tmp, linksFull, analysis, link);
      }
    }
    String contents = analysis.getContents();
    if ((internalLink == null) && !templates.isEmpty()) {
      int tmpIndex = link.getBeginIndex() + link.getTextOffset();
      while ((internalLink == null) && (tmpIndex < link.getEndIndex())) {
        if (contents.startsWith("{{", tmpIndex)) {
          PageElementTemplate template = analysis.isInTemplate(tmpIndex);
          if (template != null) {
            TemplateReplacement config = templates.get(template.getTemplateName());
            if (config != null) {
              internalLink = template;
              if (config.replacement != null) {
                if (errors == null) {
                  return true;
                }
                CheckErrorResult errorResult = createCheckErrorResult(
                    analysis, link.getBeginIndex(), link.getEndIndex());
                String replacementEnd = null;
                if (config.fullReplacement) {
                  replacementEnd =
                      config.replacement +
                      contents.substring(template.getEndIndex(), link.getEndIndex());
                } else {
                  int endTemplateName = (template.getParameterCount() > 0) ?
                      template.getParameterPipeIndex(0) :
                      template.getEndIndex() - 2;
                  replacementEnd = 
                      "{{" + config.replacement +
                      contents.substring(endTemplateName, link.getEndIndex());
                }
                String replacement =
                    contents.substring(link.getBeginIndex(), template.getBeginIndex()) +
                    replacementEnd;
                String description =
                    "[..." +
                    contents.substring(link.getLinkEndIndex(), template.getBeginIndex()) +
                    replacementEnd;
                errorResult.addReplacement(replacement, description, config.automatic);
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
    int endExtraShort = endExtra;
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
          endExtraShort = endExtra;
        }
      }
    }
    while ((beginExtra > 0) &&
        CharacterUtils.isWhitespace(contents.charAt(beginExtra - 1))) {
      beginExtra--;
    }
    boolean checkTexts = true;
    while ((checkTexts) && (link.getLinkEndIndex() <= beginExtra)) {
      String prefix = contents.substring(link.getLinkEndIndex(), beginExtra);
      int prefixLength = prefix.length();
      checkTexts = false;
      for (String[] text : textsBefore) {
        int textLength = text[0].length();
        if ((prefixLength >= textLength) &&
            prefix.substring(prefixLength - textLength).equalsIgnoreCase(text[0])) {
          int tmpIndex = beginExtra - text[0].length();
          char charBefore = contents.charAt(tmpIndex - 1);
          if ((tmpIndex <= link.getLinkEndIndex()) ||
              CharacterUtils.isWhitespace(charBefore) ||
              CharacterUtils.isInText(charBefore, PUNCTUATION_BEFORE)) {
            beginExtra = tmpIndex;
            automatic |= text.length > 1 && Boolean.parseBoolean(text[1]);
            checkTexts = true;
            continue;
          }
        }
      }
      if (!checkTexts) {
        while ((beginExtra > 0) &&
            (CharacterUtils.isWhitespace(contents.charAt(beginExtra - 1)) ||
             CharacterUtils.isInText(contents.charAt(beginExtra - 1), PUNCTUATION_BEFORE))) {
          if (CharacterUtils.isInText(contents.charAt(beginExtra - 1), AUTOMATIC_PUNCTUATION_BEFORE)) {
            automatic = true;
          }
          beginExtra--;
          checkTexts = true;
        }
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
      if (!checkTexts) {
        while ((endExtra < contents.length()) &&
            (CharacterUtils.isWhitespace(contents.charAt(endExtra)) ||
             CharacterUtils.isInText(contents.charAt(endExtra), PUNCTUATION_AFTER))) {
          if (endExtraShort == endExtra) {
            endExtraShort++;
          }
          endExtra++;
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

    // Internal link reaching both the beginning and the end of the external link
    if (closeBracket &&
        (link.getLinkEndIndex() >= beginExtra) &&
        (endExtra >= endError - 1)) {
      String replacement =
          contents.substring(internalLink.getBeginIndex(), internalLink.getEndIndex());
      errorResult.addReplacement(
          replacement,
          internalLinkFull && (endError == endExtra + 1));
    }

    // Internal link reaching the end of the external link, and text before
    if (closeBracket &&
        (link.getLinkEndIndex() < beginExtra - 1)) {
      String replacementEnd =
          "]" +
          contents.substring(beginExtra, endError - 1);
      String replacement =
          contents.substring(beginError, beginExtra) +
          replacementEnd;
      String description = replacement;
      if (link.getLinkEndIndex() < beginExtra) {
        description =
            "[..." +
            contents.substring(link.getLinkEndIndex(), beginExtra) +
            replacementEnd;
      }
      errorResult.addReplacement(
          replacement, description,
          automatic || (internalLinkLast && endError <= endExtra + 1));
    }

    // Internal link reaching the beginning of the external link
    if (link.getLinkEndIndex() >= beginExtra) {
      int tmpBeginExtra = beginExtra;
      while (CharacterUtils.isWhitespace(contents.charAt(tmpBeginExtra))) {
        tmpBeginExtra++;
      }
      String replacementBeginShort = contents.substring(tmpBeginExtra, endExtraShort);
      String replacementEndShort = contents.substring(endExtraShort, endError);
      String replacement = contents.substring(beginError, tmpBeginExtra);
      String description = "[..." + contents.substring(beginExtra, tmpBeginExtra);
      errorResult.addReplacement(
          replacementBeginShort + replacement + replacementEndShort,
          replacementBeginShort + description + replacementEndShort,
          internalLinkFirst && (endExtraShort < endError - 1));
      if (endExtra != endExtraShort) {
        String replacementBegin = contents.substring(tmpBeginExtra, endExtra);
        String replacementEnd = contents.substring(endExtra, endError);
        errorResult.addReplacement(
            replacementBegin + replacement + replacementEnd,
            replacementBegin + description + replacementEnd);
      }
    }

    // Replacing the internal link by its text
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

  /**
   * @param link Link to be checked.
   * @param list List of page names (and potential URL restrictions).
   * @param analysis Page analysis to check through redirects.
   * @param externalLink External link.
   * @return True if the link points to a page in the list.
   */
  private boolean isInList(
      PageElementInternalLink link,
      Map<String, List<String>> list,
      PageAnalysis analysis,
      PageElementExternalLink externalLink) {
    if ((link == null) || (list == null)) {
      return false;
    }
    String pageName = link.getLink();
    List<String> listUrl = null;
    boolean listFound = false;
    if (list.containsKey(pageName)) {
      listUrl = list.get(pageName);
      listFound = true;
    } else {
      if ((analysis != null) &&
          (analysis.getPage() != null) &&
          (analysis.getPage().getLinks() != null)) {
        List<Page> links = analysis.getPage().getLinks();
        for (Page tmp : links) {
          if (Page.areSameTitle(pageName, tmp.getTitle()) ) {
            PageRedirect redirect = tmp.getRedirects();
            if ((redirect != null) &&
                (redirect.getLastPage() != null) &&
                list.containsKey(redirect.getLastPage().getTitle())) {
              listUrl = list.get(redirect.getLastPage().getTitle());
              listFound = true;
            }
          }
        }
      }
    }
    if (!listFound) {
      return false;
    }
    if ((listUrl == null) || listUrl.isEmpty()) {
      return true;
    }
    if (externalLink == null) {
      return false;
    }
    return listUrl.contains(externalLink.getLink());
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

  /** List of links that can be extracted when they are at the beginning */
  private static final String PARAMETER_LINKS_FIRST = "links_first";

  /** List of links that can be extracted when they are at the end */
  private static final String PARAMETER_LINKS_LAST = "links_last";

  /** List of links that can be extracted when they are at the only thing in the link */
  private static final String PARAMETER_LINKS_FULL = "links_full";

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
            log.warn("Incorrect pattern in {} for error #513: {}", PARAMETER_TEXTS_AFTER, element);
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
          TemplateReplacement replacement = TemplateReplacement.build(tmpElement);
          if (replacement != null) {
            templates.put(replacement.templateName, replacement);
          }
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

    tmp = getSpecificProperty(PARAMETER_LINKS_FIRST, true, true, false);
    linksFirst.clear();
    if (tmp != null) {
      List<String> tmpList = WPCConfiguration.convertPropertyToStringList(tmp);
      if (tmpList != null) {
        for (String tmpElement : tmpList) {
          linksFirst.put(tmpElement, null);
        }
      }
    }

    tmp = getSpecificProperty(PARAMETER_LINKS_LAST, true, true, false);
    linksLast.clear();
    if (tmp != null) {
      List<String> tmpList = WPCConfiguration.convertPropertyToStringList(tmp);
      if (tmpList != null) {
        for (String tmpElement : tmpList) {
          linksLast.put(tmpElement, null);
        }
      }
    }

    tmp = getSpecificProperty(PARAMETER_LINKS_FULL, true, true, false);
    linksFull.clear();
    if (tmp != null) {
      List<String[]> tmpList = WPCConfiguration.convertPropertyToStringArrayList(tmp);
      if ((tmpList != null) && !tmpList.isEmpty()) {
        for (String[] tmpElement : tmpList) {
          if (tmpElement.length > 0) {
            String pageName = tmpElement[0];
            List<String> internalList = linksFull.get(pageName);
            if (internalList == null) {
              internalList = new ArrayList<>();
              linksFull.put(pageName, internalList);
            }
            if (tmpElement.length > 1) {
              internalList.add(tmpElement[1]);
            }
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
  private final Map<String, TemplateReplacement> templates = new HashMap<>();

  /** Template parameters that create an external link */
  private final Map<String, List<String[]>> templateParams = new HashMap<>();

  /** Internal links that can be extracted when at the beginning of the external link */
  private final Map<String, List<String>> linksFirst = new HashMap<>();

  /** Internal links that can be extracted when at the end of the external link */
  private final Map<String, List<String>> linksLast = new HashMap<>();

  /** Internal links that can be extracted when they are the only thing in the external link */
  private final Map<String, List<String>> linksFull = new HashMap<>();

  /** Linter category */
  private LinterCategory linterCategory = null;

  /** Bean holding configuration for template replacement */
  private static class TemplateReplacement {

    /** Name of the template */
    public final String templateName;

    /** True if the replacement should be full or only the template name */
    public final boolean fullReplacement;

    /** Replacement */
    public final String replacement;

    /** True if replacement can be automatic */
    public final boolean automatic;

    public static TemplateReplacement build(String[] data) {
      if ((data == null) || (data.length == 0)) {
        return null;
      }
      String name = data[0];
      boolean full = false;
      if (name.startsWith("{{") && name.endsWith("}}")) {
        name = name.substring(2, name.length() - 2);
        full = true;
      }
      return new TemplateReplacement(
          name, full,
          data.length > 1 ? data[1] : null,
          data.length > 2 ? Boolean.parseBoolean(data[2]) : false);
    }

    private TemplateReplacement(
        String templateName,
        boolean fullReplacement,
        String replacement,
        boolean automatic) {
      this.templateName = templateName;
      this.fullReplacement = fullReplacement;
      this.replacement = replacement;
      this.automatic = automatic;
    }
  }

  /**
   * Build the list of parameters for this algorithm.
   */
  @Override
  protected void addParameters() {
    super.addParameters();
    addParameter(new AlgorithmParameter(
        PARAMETER_LINKS_FIRST,
        GT._T("A list of links that can be extracted when at the beginning of the external link"),
        new AlgorithmParameterElement(
            "page name",
            GT._T("An internal link that can be extracted when at the beginning of the external link")),
        true));
    addParameter(new AlgorithmParameter(
        PARAMETER_LINKS_LAST,
        GT._T("A list of links that can be extracted when at the end of the external link"),
        new AlgorithmParameterElement(
            "page name",
            GT._T("An internal link that can be extracted when at the end of the external link")),
        true));
    addParameter(new AlgorithmParameter(
        PARAMETER_LINKS_FULL,
        GT._T("A list of links for which the external link can be removed it it takes all the text"),
        new AlgorithmParameterElement[] {
          new AlgorithmParameterElement(
              "page name",
              GT._T("An internal link for which the external link can be removed it it takes all the text")),
          new AlgorithmParameterElement(
              "url",
              GT._T("Value of the URL to remove the external link"),
              true)
        },
        true));
    addParameter(new AlgorithmParameter(
        PARAMETER_TEMPLATES,
        GT._T("A list of templates that create an internal link"),
        new AlgorithmParameterElement[] {
            new AlgorithmParameterElement(
                "template name",
                GT._T("Name of a template that creates an internal link")),
            new AlgorithmParameterElement(
                "replacement template",
                GT._T("Name of a template that can be used as a replacement"),
                true),
            new AlgorithmParameterElement(
                "true/false",
                GT._T("True if the replacement can be automatic"),
                true)
        },
        true));
    addParameter(new AlgorithmParameter(
        PARAMETER_TEMPLATE_PARAMS,
        GT._T("A list of template parameters that create the text of an external link"),
        new AlgorithmParameterElement[] {
            new AlgorithmParameterElement(
                "template name",
                GT._T("Name of a template that creates an external link")),
            new AlgorithmParameterElement(
                "text parameter",
                GT._T("Name of a parameter that is used to create the text of the external link")),
            new AlgorithmParameterElement(
                "true/false",
                GT._T("True if removing internal links in the text can be automatic"),
                true)
        },
        true));
    addParameter(new AlgorithmParameter(
        PARAMETER_TEXTS_AFTER,
        GT._T("A list of texts (regular expressions) that can be after the internal link"),
        new AlgorithmParameterElement(
            "pattern",
            GT._T("Regular expression that can be after the internal link")),
        true));
    addParameter(new AlgorithmParameter(
        PARAMETER_TEXTS_BEFORE,
        GT._T("A list of texts that can be before the internal link"),
        new AlgorithmParameterElement[] {
            new AlgorithmParameterElement(
                "text",
                GT._T("Text that can be before the internal link")),
            new AlgorithmParameterElement(
                "true/false",
                GT._T("True if finding this text means that the replacement can be automatic"),
                true)
        },
        true));
  }
}

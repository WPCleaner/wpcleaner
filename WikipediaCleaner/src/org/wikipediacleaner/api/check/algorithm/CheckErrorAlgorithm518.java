/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm;

import java.util.Collection;
import java.util.List;

import org.wikipediacleaner.api.API;
import org.wikipediacleaner.api.APIException;
import org.wikipediacleaner.api.APIFactory;
import org.wikipediacleaner.api.algorithm.AlgorithmParameter;
import org.wikipediacleaner.api.algorithm.AlgorithmParameterElement;
import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.Namespace;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.data.PageAnalysis;
import org.wikipediacleaner.api.data.PageElementInternalLink;
import org.wikipediacleaner.api.data.PageElementTag;
import org.wikipediacleaner.api.data.PageElementTemplate;
import org.wikipediacleaner.i18n.GT;
import org.wikipediacleaner.utils.Configuration;
import org.wikipediacleaner.utils.ConfigurationValueInteger;


/**
 * Algorithm for analyzing error 518 of check wikipedia project.
 * Error 518: nowiki tags in main namespace
 */
public class CheckErrorAlgorithm518 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm518() {
    super("<nowiki> tags");
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
    if ((analysis == null) || (analysis.getPage() == null)) {
      return false;
    }
    Integer ns = analysis.getPage().getNamespace();
    if ((ns == null) || (ns.intValue() != Namespace.MAIN)) {
      return false;
    }

    // Check each tag
    List<PageElementTag> tags = analysis.getCompleteTags(PageElementTag.TAG_WIKI_NOWIKI);
    if ((tags == null) || (tags.isEmpty())) {
      return false;
    }
    boolean result = false;
    CheckErrorAlgorithm553 algo553 = (CheckErrorAlgorithm553) CheckErrorAlgorithms.getAlgorithm(analysis.getWikipedia(), 553);
    CheckErrorAlgorithm554 algo554 = (CheckErrorAlgorithm554) CheckErrorAlgorithms.getAlgorithm(analysis.getWikipedia(), 554);
    for (PageElementTag tag : tags) {

      // Check if the tag is already detected by another algorithm
      boolean ignore = false;
      if ((algo553 != null) && !ignore) {
        ignore |= algo553.analyzeTag(analysis, null, tag);
      }
      if ((algo554 != null) && !ignore) {
        ignore |= algo554.analyzeTag(analysis, null, tag);
      }

      // Report the tag if needed
      if (!ignore) {
        if (tag.isFullTag()) {
          result |= analyzeFullTag(analysis, errors, tag);
        } else if (tag.isComplete()) {
          result |= analyzeCompleteTag(analysis, errors, tag);
        } else {
          result |= analyzePartialTag(analysis, errors, tag);
        }
      }
    }

    return result;
  }

  /**
   * Analyze a partial tag to check if errors are present.
   * 
   * @param analysis Page analysis.
   * @param errors Errors found in the page.
   * @param tag Partial tag to be checked.
   * @return Flag indicating if the error was found.
   */
  public boolean analyzePartialTag(
      PageAnalysis analysis,
      Collection<CheckErrorResult> errors,
      PageElementTag tag) {

    if (errors == null) {
      return true;
    }

    CheckErrorResult errorResult = createCheckErrorResult(
        analysis, tag.getCompleteBeginIndex(), tag.getCompleteEndIndex());
    errorResult.addReplacement("");
    errors.add(errorResult);
    return true;
  }

  /**
   * Analyze a complete tag to check if errors are present.
   * 
   * @param analysis Page analysis.
   * @param errors Errors found in the page.
   * @param tag Complete tag to be checked.
   * @return Flag indicating if the error was found.
   */
  public boolean analyzeCompleteTag(
      PageAnalysis analysis,
      Collection<CheckErrorResult> errors,
      PageElementTag tag) {

    if (errors == null) {
      return true;
    }

    // Complete tag <nowiki> ... </nowiki>
    String contents = analysis.getContents();
    CheckErrorResult errorResult = createCheckErrorResult(
        analysis, tag.getCompleteBeginIndex(), tag.getCompleteEndIndex());
    String internalText = contents.substring(
        tag.getValueBeginIndex(), tag.getValueEndIndex());

    // Check for specific characters
    StringBuilder replacement = new StringBuilder();
    for (int i = 0; i < internalText.length(); i++) {
      char currentChar = internalText.charAt(i);
      if ((apostropheTemplate != null) && (currentChar == '\'')) {
        replacement.append(PageElementTemplate.createTemplate(apostropheTemplate));
      } else if ((asteriskTemplate != null) && (currentChar == '*')) {
        replacement.append(PageElementTemplate.createTemplate(asteriskTemplate));
      } else if ((openSBTemplate != null) && (currentChar == '[')) {
          replacement.append(PageElementTemplate.createTemplate(openSBTemplate));
      } else if ((closeSBTemplate != null) && (currentChar == ']')) {
        replacement.append(PageElementTemplate.createTemplate(closeSBTemplate));
      } else {
        replacement.append(currentChar);
      }
    }
    if (!internalText.equals(replacement.toString())) {
      errorResult.addReplacement(replacement.toString());
    }

    // Check for <nowiki><tag></nowiki>
    if (internalText.startsWith("<") && internalText.endsWith(">")) {
      boolean otherFound = false;
      for (int i = 1; i < internalText.length() - 1; i++) {
        char currentChar = internalText.charAt(i);
        if ((currentChar == '<') || (currentChar == '>')) {
          otherFound = true;
        }
      }
      if (!otherFound) {
        errorResult.addReplacement("&lt;" + internalText.substring(1, internalText.length() - 1) + "&gt;");
      }
    }

    // Check for <nowiki> </nowiki> at the beginning of a line
    int begin = tag.getBeginIndex();
    if ((begin > 0) && (contents.charAt(begin - 1) == '\n')) {
      int index = 0;
      while ((index < internalText.length()) && (internalText.charAt(index) == ' ')) {
        index++;
      }
      if (index > 0) {
        internalText = internalText.substring(index);
      }
    }
    errorResult.addReplacement(internalText);

    errors.add(errorResult);
    return true;
  }

  /**
   * Analyze a full tag to check if errors are present.
   * 
   * @param analysis Page analysis.
   * @param errors Errors found in the page.
   * @param tag Full tag to be checked.
   * @return Flag indicating if the error was found.
   */
  public boolean analyzeFullTag(
      PageAnalysis analysis,
      Collection<CheckErrorResult> errors,
      PageElementTag tag) {

    // Full tag <nowiki/>
    int beginIndex = tag.getBeginIndex();
    int endIndex = tag.getEndIndex();

    // Check for <nowiki/> inside an internal link
    String contents = analysis.getContents();
    PageElementInternalLink link = analysis.isInInternalLink(beginIndex);
    if (link != null) {
      int index = beginIndex;
      while ((index > link.getBeginIndex() + link.getTextOffset()) &&
             (contents.charAt(index - 1) == ' ')) {
        index--;
      }
      if (index == link.getBeginIndex() + link.getTextOffset()) {
        index = endIndex;
        while ((index + 2 < link.getEndIndex()) &&
               (contents.charAt(index) == ' ')) {
          index++;
        }
        if (index + 2 == link.getEndIndex()) {
          CheckErrorResult errorResult = createCheckErrorResult(
              analysis, link.getBeginIndex(), link.getEndIndex());
          errorResult.addReplacement("");
          errors.add(errorResult);
          return true;
        }
      }
    }

    // Other cases
    String textBefore = contents.substring(beginIndex, tag.getBeginIndex());
    String textAfter = contents.substring(tag.getEndIndex(), endIndex);
    CheckErrorResult errorResult = createCheckErrorResult(
        analysis, beginIndex, endIndex);
    if (link != null) {
      String displayed = link.getDisplayedTextNotTrimmed();
      if (displayed.endsWith(" ")) {
        errorResult.addReplacement(PageElementInternalLink.createInternalLink(
            link.getFullLink(), displayed.trim()) + " " + textAfter);
      }
      errorResult.addReplacement(PageElementInternalLink.createInternalLink(
          link.getFullLink(), displayed + textAfter));
    }
    errorResult.addReplacement(textBefore + " " + textAfter);
    errorResult.addReplacement(textBefore + textAfter);
    errors.add(errorResult);
    return true;
  }

  /**
   * @return True if the error has a special list of pages.
   */
  @Override
  public boolean hasSpecialList() {
    return (abuseFilter != null);
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
    if (abuseFilter == null) {
      return null;
    }
    API api = APIFactory.getAPI();
    Configuration config = Configuration.getConfiguration();
    int maxDays = config.getInt(wiki, ConfigurationValueInteger.MAX_DAYS_ABUSE_LOG);
    try {
      return api.retrieveAbuseLog(wiki, abuseFilter, maxDays);
    } catch (APIException e) {
      //
    }
    return null;
  }

  /* ====================================================================== */
  /* PARAMETERS                                                             */
  /* ====================================================================== */

  /** Template replacing an apostrophe */
  private static final String PARAMETER_APOSTROPHE_TEMPLATE = "apostrophe_template";

  /** Template replacing an asterisk */
  private static final String PARAMETER_ASTERISK_TEMPLATE = "asterisk_template";

  /** Template replacing an opening square bracket */
  private static final String PARAMETER_OPEN_SB_TEMPLATE = "open_sb_template";

  /** Template replacing a closing square bracket */
  private static final String PARAMETER_CLOSE_SB_TEMPLATE = "close_sb_template";

  /** Identifier of abuse filter */
  private static final String PARAMETER_ABUSE_FILTER = "abuse_filter";

  /**
   * Initialize settings for the algorithm.
   * 
   * @see org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithmBase#initializeSettings()
   */
  @Override
  protected void initializeSettings() {
    apostropheTemplate = getSpecificProperty(PARAMETER_APOSTROPHE_TEMPLATE, true, false, false);
    asteriskTemplate = getSpecificProperty(PARAMETER_ASTERISK_TEMPLATE, true, false, false);
    openSBTemplate = getSpecificProperty(PARAMETER_OPEN_SB_TEMPLATE, true, false, false);
    closeSBTemplate = getSpecificProperty(PARAMETER_CLOSE_SB_TEMPLATE, true, false, false);

    String tmp = getSpecificProperty(PARAMETER_ABUSE_FILTER, true, true, false);
    abuseFilter = null;
    if ((tmp != null) && (tmp.trim().length() > 0)) {
      try {
        abuseFilter = Integer.valueOf(tmp);
      } catch (NumberFormatException e) {
        // Nothing to do
      }
    }
  }

  /** Template replacing an apostrophe */
  private String apostropheTemplate = null;

  /** Template replacing an asterisk */
  private String asteriskTemplate = null;

  /** Template replacing an opening square bracket */
  private String openSBTemplate = null;

  /** Template replacing a closing square bracket */
  private String closeSBTemplate = null;

  /** Identifier of abuse filter */
  private Integer abuseFilter = null;

  /**
   * Build the list of parameters for this algorithm.
   */
  @Override
  protected void addParameters() {
    super.addParameters();
    String textAbuseFilter = GT._T(
        "An identifier of an abuse filter that is triggered by {0} tags.",
        PageElementTag.TAG_WIKI_NOWIKI);
    addParameter(new AlgorithmParameter(
        PARAMETER_ABUSE_FILTER,
        textAbuseFilter,
        new AlgorithmParameterElement(
            "abuse filter identifier",
            textAbuseFilter)));
    addParameter(new AlgorithmParameter(
        PARAMETER_APOSTROPHE_TEMPLATE,
        GT._T("A template that can be used instead of an apostrophe."),
        new AlgorithmParameterElement(
            "template name",
            GT._T("A template that can be used instead of an apostrophe."))));
    addParameter(new AlgorithmParameter(
        PARAMETER_ASTERISK_TEMPLATE,
        GT._T("A template that can be used instead of an asterisk."),
        new AlgorithmParameterElement(
            "template name",
            GT._T("A template that can be used instead of an asterisk."))));
    addParameter(new AlgorithmParameter(
        PARAMETER_CLOSE_SB_TEMPLATE,
        GT._T("A template that can be used instead of a closing square bracket."),
        new AlgorithmParameterElement(
            "template name",
            GT._T("A template that can be used instead of a closing square bracket."))));
    addParameter(new AlgorithmParameter(
        PARAMETER_OPEN_SB_TEMPLATE,
        GT._T("A template that can be used instead of an opening square bracket."),
        new AlgorithmParameterElement(
            "template name",
            GT._T("A template that can be used instead of an opening square bracket."))));
  }
}

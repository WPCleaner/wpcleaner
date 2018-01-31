/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.api.data;

import java.util.ArrayList;
import java.util.List;

import org.wikipediacleaner.api.constants.WPCConfiguration;
import org.wikipediacleaner.api.constants.WPCConfigurationStringList;
import org.wikipediacleaner.api.data.PageElementTemplate.Parameter;
import org.wikipediacleaner.api.data.contents.ContentsComment;


/**
 * Class containing information about a PMID (PubMed identifier).
 */
public class PageElementPMID extends PageElement {

  /** PMID prefix */
  public final static String PMID_PREFIX = "PMID";

  /** PMID incorrect prefixes */
  private final static String[] PMID_INCORRECT_PREFIX = {
    "pmid"
  };

  /** PMID possible meaningful characters */
  public final static String POSSIBLE_CHARACTERS = "0123456789";

  /** PMID possible extraneous characters */
  public final static String EXTRA_CHARACTERS = "";

  /** PMID incorrect characters */
  private final static String INCORRECT_CHARACTERS = "- :‐\t—=–\n";

  /** PMID incorrect characters at the beginning */
  private final static String INCORRECT_BEGIN_CHARACTERS = "- :‐\t—=–\n";

  /**
   * @param analysis Page analysis.
   * @return List of PMID.
   */
  public static List<PageElementPMID> analyzePage(
      PageAnalysis analysis) {
    List<PageElementPMID> pmids = new ArrayList<PageElementPMID>();

    // Configuration
    WPCConfiguration config = analysis.getWPCConfiguration();
    List<String[]> pmidIgnoreTemplates = config.getStringArrayList(WPCConfigurationStringList.PMID_IGNORE_TEMPLATES); 

    // Search for PMID templates
    List<String[]> pmidTemplates = config.getStringArrayList(WPCConfigurationStringList.PMID_TEMPLATES);
    if (pmidTemplates != null) {
      for (String[] pmidTemplate : pmidTemplates) {
        if (pmidTemplate.length > 0) {
          String[] params = null;
          List<PageElementTemplate> templates = analysis.getTemplates(pmidTemplate[0]);
          if (templates != null) {
            for (PageElementTemplate template : templates) {
              if (params == null) {
                if (pmidTemplate.length > 1) {
                  params = pmidTemplate[1].split(",");
                } else {
                  params = new String[]{ "1" };
                }
              }
              for (String param : params) {
                if ((param != null) && (param.length() > 0)) {
                  analyzeTemplateParams(
                      analysis, pmids, pmidIgnoreTemplates,
                      template, param,
                      false, false, false, false);
                }
              }
            }
          }
        }
      }
    }

    // Search for PMID templates where help is requested
    pmidTemplates = config.getStringArrayList(WPCConfigurationStringList.PMID_HELP_NEEDED_TEMPLATES);
    if (pmidTemplates != null) {
      for (String[] pmidTemplate : pmidTemplates) {
        if (pmidTemplate.length > 0) {
          List<PageElementTemplate> templates = analysis.getTemplates(pmidTemplate[0]);
          if (templates != null) {
            for (PageElementTemplate template : templates) {
              analyzeTemplateParams(
                  analysis, pmids, pmidIgnoreTemplates,
                  template,
                  ((pmidTemplate.length > 1) && (pmidTemplate[1].length() > 0)) ? pmidTemplate[1] : "1",
                  false, false, false, true);
            }
          }
        }
      }
    }

    // Search for PMID in template parameters
    List<PageElementTemplate> templates = analysis.getTemplates();
    for (PageElementTemplate template : templates) {
      analyzeTemplateParams(
          analysis, pmids, pmidIgnoreTemplates,
          template, "PMID", true, true, true, false);
    }

    // Search for PMID in plain texts
    analyzePlainText(analysis, pmids);

    return pmids;
  }

  /**
   * @param index Current index.
   * @param pmids List of PMID.
   * @return True if the current index is already in a PMID.
   */
  private static boolean isInPMID(int index, List<PageElementPMID> pmids) {
    if (pmids != null) {
      for (PageElementPMID tmpPmid : pmids) {
        if ((tmpPmid.getBeginIndex() <= index) &&
            (tmpPmid.getEndIndex() > index)) {
          return true;
        }
      }
    }
    return false;
  }

  /**
   * Analyze plain text for PMID.
   * 
   * @param analysis Page analysis.
   * @param pmids Current list of PMID.
   */
  private static void analyzePlainText(
      PageAnalysis analysis, List<PageElementPMID> pmids) {
    String contents = analysis.getContents();
    if (contents == null) {
      return;
    }
    int index = 0;
    int maxIndex = contents.length() - 1;
    while (index < maxIndex) {
      index = checkPlainText(analysis, contents, index, pmids);
    }
  }

  /**
   * Check plain text for PMID.
   * 
   * @param analysis Page analysis.
   * @param contents Page contents.
   * @param index Current index in the page.
   * @param pmids Current list of PMID.
   * @return Next index to check.
   */
  private static int checkPlainText(
      PageAnalysis analysis, String contents, int index, List<PageElementPMID> pmids) {

    // Check special places
    if (contents.charAt(index) == '<') {
      ContentsComment comment = analysis.isInComment(index);
      if (comment != null) {
        return comment.getEndIndex();
      }
      PageElementTag tag = analysis.isInTag(index);
      if (tag != null) {
        String tagName = tag.getName();
        if (PageElementTag.TAG_WIKI_NOWIKI.equals(tagName) ||
            PageElementTag.TAG_WIKI_PRE.equals(tagName) ||
            PageElementTag.TAG_WIKI_SOURCE.equals(tagName) ||
            PageElementTag.TAG_WIKI_SYNTAXHIGHLIGHT.equals(tagName)) {
          return tag.getCompleteEndIndex();
        }
        return tag.getEndIndex();
      }
    }
    if (contents.charAt(index) == '[') {
      PageElementInterwikiLink iwLink = analysis.isInInterwikiLink(index);
      if ((iwLink != null) && (iwLink.getBeginIndex() == index)) {
        return iwLink.getEndIndex();
      }
    }

    // Check if it's a potential PMID
    String prefix = null;
    boolean correct = false;
    if (contents.startsWith(PMID_PREFIX, index)) {
      prefix = PMID_PREFIX;
      correct = true;
    }
    for (String tmpPrefix : PMID_INCORRECT_PREFIX) {
      if ((prefix == null) && (contents.length() >= index + tmpPrefix.length())) {
        String nextChars = contents.substring(index, index + tmpPrefix.length());
        if (tmpPrefix.equalsIgnoreCase(nextChars)) {
          prefix = tmpPrefix;
          correct = false;
        }
      }
    }
    if (prefix == null) {
      return index + 1;
    }

    // Manage specific locations
    if (isInPMID(index, pmids)) {
      return index + 1;
    }
    PageElementTemplate template = analysis.isInTemplate(index);
    if (template != null) {
      if (template.getParameterCount() == 0) {
        return template.getEndIndex();
      }
      int pipeIndex = template.getParameterPipeIndex(0);
      if (index < pipeIndex) {
        return pipeIndex + 1;
      }
    }
    PageElementImage image = analysis.isInImage(index);
    if (image != null) {
      int pipeIndex = image.getBeginIndex() + image.getFirstPipeOffset();
      if (index < pipeIndex) {
        return pipeIndex + 1;
      }
    }
    boolean isValid = true;
    PageElementExternalLink link = analysis.isInExternalLink(index);
    if (link != null) {
      if (!link.hasSquare() ||
          (index < link.getBeginIndex() + link.getTextOffset()) ||
          (link.getText() == null)) {
        isValid = false;
      }
    }

    // Check if it's a template parameter
    boolean parameter = false;
    if (template != null) {
      for (int paramNum = 0; paramNum < template.getParameterCount(); paramNum++) {
        if ((template.getParameterPipeIndex(paramNum) < index) &&
            (template.getParameterValueStartIndex(paramNum) > index)) {
          parameter = true;
        }
      }
    }

    int beginIndex = index;
    index += prefix.length();
    boolean isCorrect = correct;
    if (!parameter) {
      if (beginIndex >= 2) {
        if (contents.startsWith("[[", beginIndex - 2)) {
          isCorrect = false;
          beginIndex -= 2;
          if ((index + 2 < contents.length()) && contents.startsWith("]]", index)) {
            index += 2;
          }
        }
      }
      boolean spaceFound = false;
      PageElementInternalLink iLink = null;
      PageElementExternalLink eLink = null;
      if (analysis.isInComment(index) == null) {
        boolean done = false;
        while (!done) {
          done = true;
          if (index < contents.length()) {
            char currentChar = contents.charAt(index);
            if (currentChar == ' ') {
              index++;
              spaceFound = true;
              done = false;
            } else if (currentChar == '[') {
              iLink = analysis.isInInternalLink(index);
              if ((iLink != null) && (iLink.getBeginIndex() == index)) {
                isCorrect = false;
                if (iLink.getTextOffset() > 0) {
                  index += iLink.getTextOffset();
                } else {
                  index += 2;
                }
              } else {
                eLink = analysis.isInExternalLink(index);
                if ((eLink != null) && (eLink.getBeginIndex() == index)) {
                  isCorrect = false;
                  if (eLink.getTextOffset() > 0) {
                    index += eLink.getTextOffset();
                  } else {
                    index += 1;
                  }
                }
              }
            } else if (INCORRECT_BEGIN_CHARACTERS.indexOf(currentChar) >= 0) {
              index++;
              isCorrect = false;
              done = false;
            }
          }
        }
      }
      int beginNumber = -1;
      int endNumber = beginNumber;
      boolean finished = false;
      isCorrect &= spaceFound;
      boolean nextCorrect = isCorrect;
      while (!finished && (index < contents.length())) {
        char currentChar = contents.charAt(index);
        if (POSSIBLE_CHARACTERS.indexOf(currentChar) >= 0) {
          if (beginNumber < 0) {
            beginNumber = index;
          }
          endNumber = index + 1;
          index++;
          isCorrect = nextCorrect;
        } else if (EXTRA_CHARACTERS.indexOf(currentChar) >= 0) {
          if (beginNumber < 0) {
            nextCorrect = false;
          }
          index++;
        } else if (INCORRECT_CHARACTERS.indexOf(currentChar) >= 0) {
          index++;
          nextCorrect = false;
        } else {
          if ((endNumber == index) && (Character.isLetter(currentChar))) {
            isCorrect = false;
          }
          finished = true;
        }
      }
      if (endNumber > beginNumber) {
        String number = contents.substring(beginNumber, endNumber);
        if ((iLink != null) && (endNumber + 2 == iLink.getEndIndex())) {
          endNumber = iLink.getEndIndex();
        } else if ((eLink != null) && (endNumber + 1 == eLink.getEndIndex())) {
          endNumber = eLink.getEndIndex();
        } else if (contents.startsWith("[[", beginIndex) &&
                   contents.startsWith("]]", endNumber)) {
          endNumber += 2;
        }
        pmids.add(new PageElementPMID(
            beginIndex, endNumber, analysis, number,
            isValid, isCorrect, false, null));
        index = endNumber;
      } else {
        if (contents.startsWith(prefix, index) &&
            !contents.startsWith("[[PMID#", beginIndex)) {
          pmids.add(new PageElementPMID(
              beginIndex, index, analysis, "",
              isValid, false, false, null));
        }
      }
    }

    return index;
  }

  /**
   * Check if template parameter is a PMID.
   * 
   * @param analysis Page analysis.
   * @param pmids Current list of PMID.
   * @param ignoreTemplates List of templates (with parameter and value) to ignore.
   * @param template Template.
   * @param argumentName Template parameter name.
   * @param ignoreCase True if parameter name should compared ignoring case.
   * @param acceptNumbers True if numbers are accepted after parameter name.
   * @param acceptAllValues True if all values are accepted, even if not compatible with PMID. 
   * @param helpRequested True if help has been requested for this PMID.
   */
  private static void analyzeTemplateParams(
      PageAnalysis analysis, List<PageElementPMID> pmids,
      List<String[]> ignoreTemplates,
      PageElementTemplate template,
      String argumentName,
      boolean ignoreCase, boolean acceptNumbers,
      boolean acceptAllValues, boolean helpRequested) {

    // Check if template should be ignored
    if (ignoreTemplates != null) {
      for (String[] ignoreTemplate : ignoreTemplates) {
        if ((ignoreTemplate != null) &&
            (ignoreTemplate.length > 0) &&
            (Page.areSameTitle(ignoreTemplate[0], template.getTemplateName()))) {
          if (ignoreTemplate.length > 1) {
            String paramValue = template.getParameterValue(ignoreTemplate[1]);
            if (ignoreTemplate.length > 2) {
              if ((paramValue != null) &&
                  (paramValue.trim().equals(ignoreTemplate[2].trim()))) {
                return; // Ignore all templates with this name and parameter set to a given value
              }
            } else {
              if (paramValue != null) {
                return; // Ignore all templates with this name and parameter present
              }
            }
          } else {
            return; // Ignore all templates with this name
          }
        }
      }
    }

    for (int paramNum = 0; paramNum < template.getParameterCount(); paramNum++) {

      // Check parameter name
      Parameter param = template.getParameter(paramNum);
      String paramName = param.getComputedName();
      boolean nameOk = false;
      if ((ignoreCase && argumentName.equalsIgnoreCase(paramName)) ||
          (argumentName.equals(paramName))) {
        nameOk = true;
      } else if (acceptNumbers && (paramName.length() > argumentName.length())) {
        String shortParamName = paramName.substring(0, argumentName.length());
        if ((ignoreCase && argumentName.equalsIgnoreCase(shortParamName)) ||
            (argumentName.equals(paramName))) {
          nameOk = true;
          for (int i = argumentName.length(); i < paramName.length(); i++) {
            if (!Character.isDigit(paramName.charAt(i))) {
              nameOk = false;
            }
          }
        }
      }
      
      // Parameter is for a PMID, analyze its value
      if (nameOk) {
        String paramValue = param.getStrippedValue();
        boolean ok = true;
        boolean hasDigit = false;
        int i = 0;
        int beginIndex = -1;
        int endIndex = -1;
        boolean correct = true;
        while (ok && (i < paramValue.length())) {
          char currentChar = paramValue.charAt(i);
          if (POSSIBLE_CHARACTERS.indexOf(currentChar) >= 0) {
            if (Character.isDigit(currentChar)) {
              if (beginIndex < 0) {
                beginIndex = i;
              }
              endIndex = i + 1;
              hasDigit = true;
            } else if (Character.toUpperCase(currentChar) == 'X') {
              endIndex = i + 1;
            }
            i++;
          } else if (EXTRA_CHARACTERS.indexOf(currentChar) >= 0) {
            i++;
          } else if (INCORRECT_CHARACTERS.indexOf(currentChar) >= 0) {
            i++;
            correct = false;
          } else {
            ok = false;
          }
        }
        int delta = template.getParameterValueStartIndex(paramNum);
        if (beginIndex < 0) {
          beginIndex = 0;
        }
        beginIndex += delta;
        if (endIndex < 0) {
          endIndex = 0;
        }
        endIndex += delta;
        if (beginIndex < 0) {
          ok = false;
        } else {
          if (!ok && hasDigit && (paramValue.charAt(i) == '<')) {
            ContentsComment comment = analysis.isInComment(beginIndex + i);
            if ((comment != null) &&
                (comment.getBeginIndex() == beginIndex + i)) {
              ok = true;
              i += comment.getEndIndex() - comment.getBeginIndex();
              while (ok && (i < paramValue.length())) {
                char currentChar = paramValue.charAt(i);
                if (currentChar == '<') {
                  comment = analysis.isInComment(beginIndex + i);
                  if (comment != null) {
                    i += comment.getEndIndex() - comment.getBeginIndex();
                  } else {
                    ok = false;
                  }
                } else if ((currentChar != ' ') && (currentChar != '\n')) {
                  ok = false;
                } else {
                  i++;
                }
              }
            }
          }
        }
        if (ok) {
          String value = analysis.getContents().substring(beginIndex, endIndex);
          if (paramValue.length() > 0) {
            pmids.add(new PageElementPMID(
                beginIndex, endIndex, analysis, value,
                true, correct, helpRequested, template));
          }
        } else if (acceptAllValues) {
          if (paramValue.length() > 0) {
            pmids.add(new PageElementPMID(
                template.getParameterValueStartIndex(paramNum),
                template.getParameterValueStartIndex(paramNum) + paramValue.length(),
                analysis, paramValue, true, false, false, template));
          }
        }
      }
    }
  }

  /** WPCleaner configuration */
  private final WPCConfiguration wpcConfiguration;

  /** Full text */
  private final String fullText;

  /** PMID not trimmed. */
  private final String pmidNotTrimmed;

  /** PMID (trimmed). */
  private final String pmid;

  /** True if PMID is in a valid location. */
  private final boolean isValid;

  /** True if PMID syntax is correct. */
  private final boolean isCorrect;

  /** Template if PMID is a template parameter (PMID=...) */
  private final PageElementTemplate template;

  /** True if help has been requested for this PMID */
  private final boolean helpRequested;

  /**
   * @param beginIndex Begin index.
   * @param endIndex End index.
   * @param analysis Page analysis.
   * @param pmid PMID.
   * @param isValid True if PMID is in a valid location.
   * @param isCorrect True if PMID syntax is correct.
   * @param helpRequested True if help has been requested for this PMID. 
   * @param template Template if PMID is a template parameter.
   */
  private PageElementPMID(
      int beginIndex, int endIndex, PageAnalysis analysis,
      String pmid, boolean isValid,
      boolean isCorrect, boolean helpRequested,
      PageElementTemplate template) {
    super(beginIndex, endIndex);
    this.wpcConfiguration = analysis.getWPCConfiguration();
    this.fullText = analysis.getContents().substring(beginIndex, endIndex);
    this.pmidNotTrimmed = pmid;
    this.pmid = cleanPMID(pmid);
    this.isValid = isValid;
    this.isCorrect = isCorrect;
    this.helpRequested = helpRequested;
    this.template = template;
  }

  /**
   * @return PMID not trimmed.
   */
  public String getPMIDNotTrimmed() {
    return pmidNotTrimmed;
  }

  /**
   * @return PMID (trimmed).
   */
  public String getPMID() {
    return pmid;
  }

  /**
   * @return True if PMID is in a valid location.
   */
  public boolean isValid() {
    return isValid;
  }

  /**
   * @return True if PMID syntax is correct.
   */
  public boolean isCorrect() {
    return isCorrect;
  }

  /**
   * @return True if help has been requested for this PMID.
   */
  public boolean helpRequested() {
    return helpRequested;
  }

  /**
   * @return True if PMID is a template parameter.
   */
  public boolean isTemplateParameter() {
    return (template != null);
  }

  /**
   * @return List of possible PMID.
   */
  public List<String> getCorrectPMID() {
    List<String> result = new ArrayList<String>();
    String prefix = isTemplateParameter() ? "" : "PMID ";

    // Prefix outside the template
    if ((template != null) &&
        (getBeginIndex() < template.getBeginIndex())) {
      if (fullText != null) {
        result.add(fullText.substring(template.getBeginIndex() - getBeginIndex()));
      }
      return result;
    }

    // Construct a basic PMID number
    StringBuilder buffer = new StringBuilder();
    for (int i = 0; i < pmidNotTrimmed.length(); i++) {
      char currentChar = pmidNotTrimmed.charAt(i);
      if ((POSSIBLE_CHARACTERS.indexOf(currentChar) >= 0) ||
          (EXTRA_CHARACTERS.indexOf(currentChar) >= 0)) {
        buffer.append(currentChar);
      } else if (currentChar == '\t') {
        buffer.append(" ");
      } else {
        buffer.append(currentChar);
      }
    }
    String cleanedPMID = buffer.toString().trim();

    // Basic replacement
    addCorrectPMID(result, prefix, cleanedPMID);
    
    return result;
  }

  /**
   * @param result List of possible replacements.
   * @param prefix PMI prefix.
   * @param cleanedPMID Cleaned up PMID.
   */
  private void addCorrectPMID(List<String> result, String prefix, String cleanedPMID) {
    addCorrectPMID(result, prefix + cleanedPMID);
    if (!isTemplateParameter()) {
      List<String[]> pmidTemplates = wpcConfiguration.getStringArrayList(
          WPCConfigurationStringList.PMID_TEMPLATES);
      if (pmidTemplates != null) {
        for (String[] pmidTemplate : pmidTemplates) {
          if (pmidTemplate.length > 2) {
            String[] params = pmidTemplate[1].split(",");
            Boolean suggested = Boolean.valueOf(pmidTemplate[2]);
            if ((params.length > 0) && (Boolean.TRUE.equals(suggested))) {
              StringBuilder buffer = new StringBuilder();
              buffer.append("{{");
              buffer.append(pmidTemplate[0]);
              buffer.append("|");
              if (!"1".equals(params[0])) {
                buffer.append(params[0]);
                buffer.append("=");
              }
              buffer.append(cleanedPMID);
              buffer.append("}}");
              addCorrectPMID(result, buffer.toString());
            }
          }
        }
      }
      
    }
  }

  /**
   * @param result List of possible replacements.
   * @param correctPMID Possible replacement.
   */
  private void addCorrectPMID(List<String> result, String correctPMID) {
    if ((result == null) || (correctPMID == null)) {
      return;
    }
    if (!result.contains(correctPMID)) {
      result.add(correctPMID);
    }
  }

  /**
   * @param helpNeededTemplate Name of template for asking for help.
   * @param reason Reason of the request.
   * @return Text for requesting for help.
   */
  public String askForHelp(
      String[] helpNeededTemplate, String reason) {
    if ((helpNeededTemplate == null) ||
        (helpNeededTemplate.length == 0)) {
      return null;
    }
    if (isTemplateParameter()) {
      return null;
    }

    // Template name
    StringBuilder replacement = new StringBuilder();
    replacement.append("{{");
    replacement.append(helpNeededTemplate[0]);

    // PMID
    replacement.append("|");
    if ((helpNeededTemplate.length > 1) &&
        (helpNeededTemplate[1].length() > 0)) {
      replacement.append(helpNeededTemplate[1]);
      replacement.append("=");
    }
    replacement.append(getPMIDNotTrimmed());

    // Reason
    if ((reason != null) &&
        (helpNeededTemplate.length > 2) &&
        (helpNeededTemplate[2].length() > 0)) {
      replacement.append("|");
      replacement.append(helpNeededTemplate[2]);
      replacement.append("=");
      replacement.append(reason);
    }

    // Extra parameters
    for (int i = 3; i < helpNeededTemplate.length; i++) {
      replacement.append("|");
      replacement.append(helpNeededTemplate[i]);
    }

    replacement.append("}}");
    return replacement.toString();
  }

  /**
   * @param comment Comment for asking for help.
   * @param reason Reason of the request.
   * @return Text for requesting for help.
   */
  public String askForHelp(
      String comment, String reason) {
    if ((comment == null) ||
        (comment.trim().length() == 0)) {
      return null;
    }
    StringBuilder replacement = new StringBuilder();
    replacement.append("<!-- ");
    replacement.append(comment);
    if ((reason != null) && (reason.trim().length() > 0)) {
      replacement.append(" - ");
      replacement.append(reason);
    }
    replacement.append(" -->");
    return replacement.toString();
  }

  /**
   * @param pmid PMID number.
   * @return Cleaned up PMID number.
   */
  public static String cleanPMID(String pmid) {
    if (pmid == null) {
      return null;
    }
    pmid = pmid.trim();
    if (pmid.length() == 0) {
      return pmid;
    }
    PageAnalysis analysis = new PageAnalysis(null, pmid);
    StringBuilder result = new StringBuilder();
    int i = 0;
    while (i < pmid.length()) {
      char current = Character.toUpperCase(pmid.charAt(i));
      if (current == '<') {
        ContentsComment comment = analysis.isInComment(i);
        if ((comment != null) && (comment.getBeginIndex() == i)) {
          i = comment.getEndIndex() - 1;
        } else {
          PageElementTag refTag = analysis.isInTag(i, PageElementTag.TAG_WIKI_REF);
          if ((refTag != null) && (refTag.getBeginIndex() == i)) {
            i = refTag.getCompleteEndIndex() - 1;
          }
        }
      } else if (POSSIBLE_CHARACTERS.indexOf(current) >= 0) {
        result.append(current);
      }
      i++;
    }
    return result.toString();
  }

  /**
   * @param pmidValue PMID value.
   * @return True if PMID value is valid.
   */
  public static boolean isValid(String pmidValue) {
    if (pmidValue == null) {
      return false;
    }
    pmidValue = cleanPMID(pmidValue);
    if (pmidValue.length() == 0) {
      return false;
    }
    return true;
  }
}

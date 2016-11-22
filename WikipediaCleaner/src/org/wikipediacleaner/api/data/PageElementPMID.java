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


/**
 * Class containing information about a PMID (PubMed identifier).
 */
public class PageElementPMID extends PageElement {

  /** PMID prefix */
  private final static String PMID_PREFIX = "PMID";

  /** PMID incorrect prefixes */
  private final static String[] PMID_INCORRECT_PREFIX = {
    "pmid"
  };

  /** PMID possible meaningful characters */
  private final static String POSSIBLE_CHARACTERS = "0123456789";

  /** PMID possible extraneous characters */
  private final static String EXTRA_CHARACTERS = "";

  /** PMID incorrect characters */
  private final static String INCORRECT_CHARACTERS = "- :‐\t—=–\n";

  /** ISBN incorrect characters at the beginning */
  private final static String INCORRECT_BEGIN_CHARACTERS = "- :‐\t—=–\n";

  /**
   * @param analysis Page analysis.
   * @return List of PMID.
   */
  public static List<PageElementPMID> analyzePage(
      PageAnalysis analysis) {
    List<PageElementPMID> pmids = new ArrayList<PageElementPMID>();

    // Search for PMID templates
    WPCConfiguration config = analysis.getWPCConfiguration();
    List<String[]> pmidTemplates = config.getStringArrayList(WPCConfigurationStringList.PMID_TEMPLATES);
    if (pmidTemplates != null) {
      for (String[] pmidTemplate : pmidTemplates) {
        if (pmidTemplate.length > 0) {
          List<PageElementTemplate> templates = analysis.getTemplates(pmidTemplate[0]);
          if (templates != null) {
            for (PageElementTemplate template : templates) {
              analyzeTemplateParams(
                  analysis, pmids, template,
                  (pmidTemplate.length > 1) ? pmidTemplate[1] : "1",
                  false, false, false, false);
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
                  analysis, pmids, template,
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
      analyzeTemplateParams(analysis, pmids, template, "PMID", true, true, true, false);
    }

    // Search for PMID in plain texts
    analyzePlainText(analysis, pmids, PMID_PREFIX, true, true);
    for (String prefix : PMID_INCORRECT_PREFIX) {
      analyzePlainText(analysis, pmids, prefix, false, false);
    }

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
   * @param isbns Current list of PMID.
   * @param prefix PMID prefix.
   * @param correct True if PMID should be considered correct by default.
   * @param caseSensitive True if PMID prefix is case sensitive.
   */
  private static void analyzePlainText(
      PageAnalysis analysis, List<PageElementPMID> pmids,
      String prefix, boolean correct, boolean caseSensitive) {
    String contents = analysis.getContents();
    if ((contents == null) || (prefix == null)) {
      return;
    }
    int index = 0;
    int maxIndex = contents.length() - prefix.length();
    while (index < maxIndex) {

      // Check if it's a potential PMID
      boolean isValid = true;
      String nextChars = contents.substring(index, index + prefix.length());
      boolean isPMID = caseSensitive ?
          prefix.equals(nextChars) : prefix.equalsIgnoreCase(nextChars);
      if (isPMID && (analysis.isInComment(index) != null)) {
        isPMID = false;
      }
      if (isPMID && (analysis.isInTag(index) != null)) {
        isPMID = false;
      }
      if (isPMID && (analysis.getSurroundingTag(PageElementTag.TAG_WIKI_NOWIKI, index) != null)) {
        isPMID = false;
      }
      if (isPMID) {
        if ((analysis.getSurroundingTag(PageElementTag.TAG_WIKI_PRE, index) != null) ||
            (analysis.getSurroundingTag(PageElementTag.TAG_WIKI_SOURCE, index) != null) ||
            (analysis.getSurroundingTag(PageElementTag.TAG_WIKI_SYNTAXHIGHLIGHT, index) != null)) {
          isPMID = false;
        }
      }
      if (isPMID && isInPMID(index, pmids)) {
        isPMID = false;
      }
      if (isPMID) {
        PageElementExternalLink link = analysis.isInExternalLink(index);
        if (link != null) {
          if (!link.hasSquare() ||
              (index < link.getBeginIndex() + link.getTextOffset()) ||
              (link.getText() == null)) {
            isValid = false;
          }
        }
      }
      if (isPMID) {
        PageElementTemplate template = analysis.isInTemplate(index);
        if (template != null) {
          if ((template.getParameterCount() == 0) ||
              (index < template.getParameterPipeIndex(0))) {
            isPMID = false;
          }
        }
      }
      if (isPMID) {
        PageElementImage image = analysis.isInImage(index);
        if (image != null) {
          if (index < image.getBeginIndex() + image.getFirstPipeOffset()) {
            isPMID = false;
          }
        }
      }

      if (isPMID) {

        // Check if it's a template parameter
        boolean parameter = false;
        PageElementTemplate template = analysis.isInTemplate(index);
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
          if ((beginIndex >= 2) && (index + 2 < contents.length())) {
            if (contents.startsWith("[[", beginIndex - 2) &&
                contents.startsWith("]]", index)) {
              isCorrect = false;
              beginIndex -= 2;
              index += 2;
            }
          }
          boolean spaceFound = false;
          if (analysis.isInComment(index) == null) {
            while ((index < contents.length()) &&
                (" \u00A0".indexOf(contents.charAt(index)) >= 0)) {
              index++;
              spaceFound = true;
            }
            while ((index < contents.length()) &&
                (INCORRECT_BEGIN_CHARACTERS.indexOf(contents.charAt(index)) >= 0)) {
              index++;
              isCorrect = false;
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
            pmids.add(new PageElementPMID(
                beginIndex, endNumber, number,
                isValid, isCorrect, false, false));
            index = endNumber;
          }
        }
      } else {
        index++;
      }
    }
  }

  /**
   * Check if template parameter is a PMID.
   * 
   * @param analysis Page analysis.
   * @param pmids Current list of PMID.
   * @param template Template.
   * @param argumentName Template parameter name.
   * @param ignoreCase True if parameter name should compared ignoring case.
   * @param acceptNumbers True if numbers are accepted after parameter name.
   * @param acceptAllValues True if all values are accepted, even if not compatible with ISBN. 
   * @param helpRequested True if help has been requested for this ISBN.
   */
  private static void analyzeTemplateParams(
      PageAnalysis analysis, List<PageElementPMID> pmids,
      PageElementTemplate template,
      String argumentName,
      boolean ignoreCase, boolean acceptNumbers,
      boolean acceptAllValues, boolean helpRequested) {
    int paramDefaultName = 1;
    for (int paramNum = 0; paramNum < template.getParameterCount(); paramNum++) {

      // Check parameter name
      Parameter param = template.getParameter(paramNum);
      String paramName = param.getComputedName();
      if ((paramName == null) || (paramName.trim().length() == 0)) {
        paramName = Integer.toString(paramDefaultName);
        paramDefaultName++;
      }
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
            PageElementComment comment = analysis.isInComment(beginIndex + i);
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
                beginIndex, endIndex, value, true, correct, helpRequested, true));
          }
        } else if (acceptAllValues) {
          if (paramValue.length() > 0) {
            pmids.add(new PageElementPMID(
                template.getParameterValueStartIndex(paramNum),
                template.getParameterValueStartIndex(paramNum) + paramValue.length(),
                paramValue, true, false, false, true));
          }
        }
      }
    }
  }

  /**
   * PMID not trimmed.
   */
  private final String pmidNotTrimmed;

  /**
   * PMID (trimmed).
   */
  private final String pmid;

  /**
   * True if ISBN is in a valid location.
   */
  private final boolean isValid;

  /**
   * True if PMID syntax is correct.
   */
  private final boolean isCorrect;

  /**
   * True if PMID is a template parameter (PMID=...)
   */
  private final boolean isTemplateParameter;

  /**
   * True if help has been requested for this PMID
   */
  private final boolean helpRequested;

  /**
   * @param beginIndex Begin index.
   * @param endIndex End index.
   * @param pmid PMID.
   * @param isValid True if PMID is in a valid location.
   * @param isCorrect True if PMID syntax is correct.
   * @param helpRequested True if help has been requested for this PMID. 
   * @param isTemplateParameter True if PMID is a template parameter.
   */
  private PageElementPMID(
      int beginIndex, int endIndex,
      String pmid, boolean isValid,
      boolean isCorrect, boolean helpRequested,
      boolean isTemplateParameter) {
    super(beginIndex, endIndex);
    this.pmidNotTrimmed = pmid;
    this.pmid = cleanPMID(pmid);
    this.isValid = isValid;
    this.isCorrect = isCorrect;
    this.helpRequested = helpRequested;
    this.isTemplateParameter = isTemplateParameter;
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
    return isTemplateParameter;
  }

  /**
   * @return List of possible PMID.
   */
  public List<String> getCorrectPMID() {
    List<String> result = new ArrayList<String>();
    String prefix = isTemplateParameter ? "" : "PMID ";

    // Construct a basic PMID number
    StringBuilder buffer = new StringBuilder();
    for (int i = 0; i < pmidNotTrimmed.length(); i++) {
      char currentChar = pmidNotTrimmed.charAt(i);
      if ((POSSIBLE_CHARACTERS.indexOf(currentChar) >= 0) ||
          (EXTRA_CHARACTERS.indexOf(currentChar) >= 0)) {
        buffer.append(currentChar);
      } else if ((currentChar == '‐') ||
                 (currentChar == '.')) {
        buffer.append("-");
      } else if (currentChar == '\t') {
        buffer.append(" ");
      } else {
        buffer.append(currentChar);
      }
    }
    String cleanedPMID = buffer.toString().trim();

    // Basic replacement
    result.add(prefix + cleanedPMID);
    
    return result;
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
    if (isTemplateParameter) {
      return null;
    }

    // Template name
    StringBuilder replacement = new StringBuilder();
    replacement.append("{{");
    replacement.append(helpNeededTemplate[0]);

    // ISBN
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
        PageElementComment comment = analysis.isInComment(i);
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

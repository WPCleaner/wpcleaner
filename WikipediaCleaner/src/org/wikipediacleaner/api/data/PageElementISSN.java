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
 * Class containing information about an ISSN.
 */
public class PageElementISSN extends PageElement {

  /**
   * ISSN prefix.
   */
  private final static String ISSN_PREFIX = "ISSN";

  /**
   * ISSN possible meaningful characters.
   */
  private final static String POSSIBLE_CHARACTERS = "0123456789Xx";

  /**
   * ISSN possible extraneous characters.
   */
  private final static String EXTRA_CHARACTERS = "-";

  /**
   * ISSN incorrect characters.
   */
  private final static String INCORRECT_CHARACTERS = ":‐\t—=–#  ";

  /**
   * ISSN incorrect characters at the beginning.
   */
  private final static String INCORRECT_BEGIN_CHARACTERS = ":;‐\t—=–#";

  /**
   * @param analysis Page analysis.
   * @return List of ISSN.
   */
  public static List<PageElementISSN> analyzePage(
      PageAnalysis analysis) {
    List<PageElementISSN> issns = new ArrayList<PageElementISSN>();

    // Configuration
    WPCConfiguration config = analysis.getWPCConfiguration();
    List<String[]> issnIgnoreTemplates = config.getStringArrayList(WPCConfigurationStringList.ISSN_IGNORE_TEMPLATES); 

    // Search for ISSN templates
    List<String[]> issnTemplates = config.getStringArrayList(WPCConfigurationStringList.ISSN_TEMPLATES);
    if (issnTemplates != null) {
      for (String[] issnTemplate : issnTemplates) {
        if (issnTemplate.length > 0) {
          List<PageElementTemplate> templates = analysis.getTemplates(issnTemplate[0]);
          if (templates != null) {
            for (PageElementTemplate template : templates) {
              String[] params = null;
              if (issnTemplate.length > 1) {
                params = issnTemplate[1].split(",");
              } else {
                params = new String[]{ "1" };
              }
              for (String param : params) {
                if ((param != null) && (param.length() > 0)) {
                  analyzeTemplateParams(
                      analysis, issns, issnIgnoreTemplates,
                      template, param,
                      false, false, true, false);
                }
              }
            }
          }
        }
      }
    }

    // Search for ISSN templates where help is requested
    issnTemplates = config.getStringArrayList(WPCConfigurationStringList.ISSN_HELP_NEEDED_TEMPLATES);
    if (issnTemplates != null) {
      for (String[] issnTemplate : issnTemplates) {
        if (issnTemplate.length > 0) {
          List<PageElementTemplate> templates = analysis.getTemplates(issnTemplate[0]);
          if (templates != null) {
            for (PageElementTemplate template : templates) {
              analyzeTemplateParams(
                  analysis, issns, issnIgnoreTemplates,
                  template,
                  ((issnTemplate.length > 1) && (issnTemplate[1].length() > 0)) ? issnTemplate[1] : "1",
                  false, false, false, true);
            }
          }
        }
      }
    }

    // Search for ISSN in template parameters
    List<PageElementTemplate> templates = analysis.getTemplates();
    for (PageElementTemplate template : templates) {
      analyzeTemplateParams(
          analysis, issns, issnIgnoreTemplates,
          template, "ISSN", true, true, true, false);
    }

    // Search for ISSN in plain texts
    String contents = analysis.getContents();
    if (contents == null) {
      return issns;
    }
    int index = 0;
    int maxIndex = contents.length() - ISSN_PREFIX.length();
    while (index < maxIndex) {

      // Check if it's a potential ISSN
      boolean isValid = true;
      boolean isISSN = ISSN_PREFIX.equalsIgnoreCase(
          contents.substring(index, index + ISSN_PREFIX.length()));
      if (isISSN && (analysis.isInComment(index) != null)) {
        isISSN = false;
      }
      if (isISSN && (analysis.isInTag(index) != null)) {
        isISSN = false;
      }
      if (isISSN && (analysis.getSurroundingTag(PageElementTag.TAG_WIKI_NOWIKI, index) != null)) {
        isISSN = false;
      }
      if (isISSN && isInISSN(index, issns)) {
        isISSN = false; // to avoid issn=ISSN xxxx-xxxx being detected twice
      }
      if (isISSN) {
        PageElementExternalLink link = analysis.isInExternalLink(index);
        if (link != null) {
          if (!link.hasSquare() ||
              (index < link.getBeginIndex() + link.getTextOffset()) ||
              (link.getText() == null)) {
            isValid = false;
          }
        }
      }
      if (isISSN) {
        PageElementTemplate template = analysis.isInTemplate(index);
        if (template != null) {
          if ((template.getParameterCount() == 0) ||
              (index < template.getParameterPipeIndex(0))) {
            isISSN = false;
          }
        }
      }
      if (isISSN) {
        PageElementImage image = analysis.isInImage(index);
        if (image != null) {
          if (index < image.getBeginIndex() + image.getFirstPipeOffset()) {
            isISSN = false;
          }
        }
      }

      if (isISSN) {

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
        index += ISSN_PREFIX.length();
        if (!parameter) {
          boolean correct = true;
          if ((beginIndex >= 2) && (index + 2 < contents.length())) {
            if (contents.startsWith("[[", beginIndex - 2) &&
                contents.startsWith("]]", index)) {
              correct = false;
              beginIndex -= 2;
              index += 2;
            }
          }
          boolean spaceFound = false;
          if (analysis.isInComment(index) == null) {
            while ((index < contents.length()) && (contents.charAt(index) == ' ')) {
              index++;
              spaceFound = true;
            }
            while ((index < contents.length()) &&
                (INCORRECT_BEGIN_CHARACTERS.indexOf(contents.charAt(index)) >= 0)) {
              index++;
              correct = false;
            }
          }
          int beginNumber = -1;
          int endNumber = beginNumber;
          boolean finished = false;
          correct &= spaceFound;
          boolean nextCorrect = correct;
          int digitCount = 0;
          boolean hasSeparator = false;
          boolean hasExtraSeparator = false;
          while (!finished && (index < contents.length())) {
            char currentChar = contents.charAt(index);
            if (POSSIBLE_CHARACTERS.indexOf(currentChar) >= 0) {
              if (beginNumber < 0) {
                beginNumber = index;
              }
              endNumber = index + 1;
              index++;
              digitCount++;
              if (!Character.isDigit(currentChar) && (digitCount != 8)) {
                correct = false;
              }
              correct &= nextCorrect;
            } else if (EXTRA_CHARACTERS.indexOf(currentChar) >= 0) {
              if (beginNumber < 0) {
                nextCorrect = false;
              } else if ((digitCount == 4) && !hasSeparator) {
                hasSeparator = true;
              } else {
                hasExtraSeparator = true;
              }
              index++;
            } else if (INCORRECT_CHARACTERS.indexOf(currentChar) >= 0) {
              index++;
              nextCorrect = false;
            } else {
              if ((endNumber == index) && (Character.isLetter(currentChar))) {
                correct = false;
              }
              finished = true;
            }
          }
          if (digitCount == 8) {
            if (!hasSeparator || hasExtraSeparator) {
              correct = false;
            }
          }
          if (endNumber > beginNumber) {
            String number = contents.substring(beginNumber, endNumber);
            issns.add(new PageElementISSN(
                beginIndex, endNumber, analysis, number,
                isValid, correct, false, null));
            index = endNumber;
          }
        }
      } else {
        index++;
      }
    }

    return issns;
  }

  /**
   * @param index Current index.
   * @param issns List of ISSN.
   * @return True if the current index is already in a ISSN.
   */
  private static boolean isInISSN(int index, List<PageElementISSN> issns) {
    if (issns != null) {
      for (PageElementISSN tmpIssn : issns) {
        if ((tmpIssn.getBeginIndex() <= index) &&
            (tmpIssn.getEndIndex() > index)) {
          return true;
        }
      }
    }
    return false;
  }

  /**
   * Check if template parameter is an ISSN.
   * 
   * @param analysis Page analysis.
   * @param issns Current list of ISSN.
   * @param ignoreTemplates List of templates (with parameter and value) to ignore.
   * @param template Template.
   * @param argumentName Template parameter name.
   * @param ignoreCase True if parameter name should compared ignoring case.
   * @param acceptNumbers True if numbers are accepted after parameter name.
   * @param acceptAllValues True if all values are accepted, even if not compatible with ISSN. 
   * @param helpRequested True if help has been requested for this ISSN.
   */
  private static void analyzeTemplateParams(
      PageAnalysis analysis, List<PageElementISSN> issns,
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

    int paramDefaultName = 1;
    for (int paramNum = 0; paramNum < template.getParameterCount(); paramNum++) {

      // Check parameter name
      Parameter param = template.getParameter(paramNum);
      String paramName = param.getName();
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

      // Parameter is for an ISSN, analyze that it's not filled by WikiData
      if (nameOk) {
        String paramValue = param.getValue();
        if ((paramValue == null) ||
            (paramValue.trim().length() == 0) ||
            "{{#property:p236}}".equalsIgnoreCase(paramValue.trim())) {
          nameOk = false;
        }
      }

      // Parameter is for an ISSN, analyze its value
      if (nameOk) {
        String paramValue = param.getValue();
        int delta = param.getValueStartIndex();
        int i = 0;
        int beginIndex = -1;
        int endIndex = -1;
        int digitCount = 0;
        boolean hasSeparator = false;
        boolean hasExtraSeparator = false;
        boolean hasExtraCharacters = false;
        boolean ok = true;
        boolean correct = true;
        boolean isEmpty = true;
        while (ok && (i < paramValue.length())) {
          char currentChar = paramValue.charAt(i);
          if (currentChar == '<') {
            PageElementComment comment = analysis.isInComment(delta + i + 1);
            if ((comment != null) && (comment.getBeginIndex() == delta + i)) {
              i += comment.getEndIndex() - comment.getBeginIndex();
            } else {
              ok = false;
              isEmpty = false;
            }
          } else if (" \n".indexOf(currentChar) >= 0) {
            isEmpty = false;
            i++;
            if (beginIndex >= 0) {
              hasExtraCharacters = true;
            }
          } else if (POSSIBLE_CHARACTERS.indexOf(currentChar) >= 0) {
            isEmpty = false;
            if (hasExtraCharacters) {
              correct = false;
            }
            if (Character.isDigit(currentChar)) {
              digitCount++;
              if (beginIndex < 0) {
                beginIndex = i;
              }
              endIndex = i + 1;
            } else if (Character.toUpperCase(currentChar) == 'X') {
              endIndex = i + 1;
              digitCount++;
              if (digitCount != 8) {
                correct = false;
              }
            }
            i++;
          } else if (EXTRA_CHARACTERS.indexOf(currentChar) >= 0) {
            isEmpty = false;
            i++;
            // Only one separation character after 4th digit
            if ((digitCount == 4) && !hasSeparator) {
              hasSeparator = true;
            } else {
              hasExtraSeparator = true;
            }
          } else if (INCORRECT_CHARACTERS.indexOf(currentChar) >= 0) {
            isEmpty = false;
            i++;
            correct = false;
          } else {
            isEmpty = false;
            ok = false;
          }
        }
        if ((beginIndex < 0) || (endIndex < 0)) {
          ok = false;
        }
        if (digitCount == 8) {
          if (!hasSeparator || hasExtraSeparator) {
            correct = false;
          }
        }
        beginIndex += delta;
        endIndex += delta;
        if (ok) {
          String contents = analysis.getContents();
          String value = contents.substring(beginIndex, endIndex);

          if (paramValue.length() > 0) {
            issns.add(new PageElementISSN(
                beginIndex, endIndex, analysis, value,
                true, correct, helpRequested, template));
          }
        } else if (acceptAllValues) {
          if (!isEmpty) {
            issns.add(new PageElementISSN(
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

  /** ISSN not trimmed */
  private final String issnNotTrimmed;

  /** ISSN (trimmed) */
  private final String issn;

  /** True if ISSN is in a valid location */
  private final boolean isValid;

  /** True if ISSN syntax is correct */
  private final boolean isCorrect;

  /** Template if ISSN is a template parameter (ISSN=...) */
  private final PageElementTemplate template;

  /** True if help has been requested for this ISSN */
  private final boolean helpRequested;

  /**
   * @param beginIndex Begin index.
   * @param endIndex End index.
   * @param issn ISSN.
   * @param isValid True if ISSN is in a valid location.
   * @param isCorrect True if ISSN syntax is correct.
   * @param helpRequested True if help has been requested for this ISSN. 
   * @param template Template if ISSN is a template parameter.
   */
  private PageElementISSN(
      int beginIndex, int endIndex, PageAnalysis analysis,
      String issn, boolean isValid,
      boolean isCorrect, boolean helpRequested,
      PageElementTemplate template) {
    super(beginIndex, endIndex);
    this.wpcConfiguration = analysis.getWPCConfiguration();
    this.fullText = analysis.getContents().substring(beginIndex, endIndex);
    this.issnNotTrimmed = issn;
    this.issn = cleanISSN(issn);
    this.isValid = isValid;
    this.isCorrect = isCorrect;
    this.helpRequested = helpRequested;
    this.template = template;
  }

  /**
   * @return ISSN not trimmed.
   */
  public String getISSNNotTrimmed() {
    return issnNotTrimmed;
  }

  /**
   * @return ISSN (trimmed).
   */
  public String getISSN() {
    return issn;
  }

  /**
   * @return True if ISSN is in a valid location.
   */
  public boolean isValid() {
    return isValid;
  }

  /**
   * @return True if ISSN syntax is correct.
   */
  public boolean isCorrect() {
    return isCorrect;
  }

  /**
   * @return True if help has been requested for this ISSN.
   */
  public boolean helpRequested() {
    return helpRequested;
  }

  /**
   * @return True if ISSN is a template parameter.
   */
  public boolean isTemplateParameter() {
    return (template != null);
  }

  /**
   * @return List of possible ISSN.
   */
  public List<String> getCorrectISSN() {
    List<String> result = new ArrayList<String>();
    String prefix = isTemplateParameter() ? "" : "ISSN ";

    // Prefix outside the template
    if ((template != null) &&
        (getBeginIndex() < template.getBeginIndex())) {
      if (fullText != null) {
        result.add(fullText.substring(template.getBeginIndex() - getBeginIndex()));
      }
      return result;
    }

    // Construct a basic ISSN number
    String tmpISSN = issnNotTrimmed.trim();
    if (tmpISSN.startsWith(ISSN_PREFIX)) {
      tmpISSN = tmpISSN.substring(ISSN_PREFIX.length()).trim();
    }
    StringBuilder buffer = new StringBuilder();
    for (int i = 0; i < tmpISSN.length(); i++) {
      char currentChar = tmpISSN.charAt(i);
      if (POSSIBLE_CHARACTERS.indexOf(currentChar) >= 0) {
        buffer.append(currentChar);
      } else if (EXTRA_CHARACTERS.indexOf(currentChar) >= 0) {
        // Nothing to add
      } else if ((currentChar == '‐') ||
                 (currentChar == '–') ||
                 (currentChar == '.')) {
        // Nothing to add
      } else if ((currentChar == '\t') ||
                 (currentChar == ' ')) {
        // Nothing to add
      } else {
        buffer.append(currentChar);
      }
    }
    if (buffer.length() == 8) {
      buffer.insert(4, '-');
    }
    String cleanedISSN = buffer.toString().trim();

    // Basic replacement
    addCorrectISSN(result, prefix, cleanedISSN);

    // Common mistyped characters
    cleanedISSN = cleanedISSN.replaceAll("x", "X");
    cleanedISSN = cleanedISSN.replaceAll("O", "0");
    cleanedISSN = cleanedISSN.replaceAll("I", "1");
    cleanedISSN = cleanedISSN.replaceAll("B", "8");
    addCorrectISSN(result, prefix, cleanedISSN);

    return result;
  }

  /**
   * @param result List of possible replacements.
   * @param prefix ISSN prefix.
   * @param cleanedISSN Cleaned up ISSN.
   */
  private void addCorrectISSN(List<String> result, String prefix, String cleanedISSN) {
    if (computeChecksum(cleanedISSN) != cleanedISSN.charAt(cleanedISSN.length() - 1)) {
      return;
    }
    addCorrectISSN(result, prefix + cleanedISSN);
    if (!isTemplateParameter()) {
      List<String[]> issnTemplates = wpcConfiguration.getStringArrayList(
          WPCConfigurationStringList.ISSN_TEMPLATES);
      if (issnTemplates != null) {
        for (String[] issnTemplate : issnTemplates) {
          if (issnTemplate.length > 2) {
            String[] params = issnTemplate[1].split(",");
            Boolean suggested = Boolean.valueOf(issnTemplate[2]);
            if ((params.length > 0) && (Boolean.TRUE.equals(suggested))) {
              StringBuilder buffer = new StringBuilder();
              buffer.append("{{");
              buffer.append(issnTemplate[0]);
              buffer.append("|");
              if (!"1".equals(params[0])) {
                buffer.append(params[0]);
                buffer.append("=");
              }
              buffer.append(cleanedISSN);
              buffer.append("}}");
              addCorrectISSN(result, buffer.toString());
            }
          }
        }
      }
    }
  }

  /**
   * @param result List of possible replacements.
   * @param correctISSN Possible replacement.
   */
  private void addCorrectISSN(List<String> result, String correctISSN) {
    if ((result == null) || (correctISSN == null)) {
      return;
    }
    if (!result.contains(correctISSN)) {
      result.add(correctISSN);
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

    // ISSN
    replacement.append("|");
    if ((helpNeededTemplate.length > 1) &&
        (helpNeededTemplate[1].length() > 0)) {
      replacement.append(helpNeededTemplate[1]);
      replacement.append("=");
    }
    replacement.append(getISSNNotTrimmed());

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
   * @param issn ISSN number.
   * @return Cleaned up ISSN number.
   */
  public static String cleanISSN(String issn) {
    if (issn == null) {
      return null;
    }
    issn = issn.trim();
    if (issn.length() == 0) {
      return issn;
    }
    PageAnalysis analysis = new PageAnalysis(null, issn);
    StringBuilder result = new StringBuilder();
    int i = 0;
    while (i < issn.length()) {
      char current = Character.toUpperCase(issn.charAt(i));
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
   * @param issnValue ISSN value.
   * @return Computed checksum.
   */
  public static char computeChecksum(String issnValue) {
    if (issnValue == null) {
      return 0;
    }
    issnValue = cleanISSN(issnValue);

    // Check for ISSN-8
    if (issnValue.length() == 8) {
      int check = 0;
      for (int i = 0; i < 7; i++) {
        char currentChar = issnValue.charAt(i);
        if (Character.isDigit(currentChar)) {
          check += (8 - i) * (currentChar - '0');
        } else {
          return 0;
        }
      }
      check = check % 11; // Modulus 11
      check = 11 - check; // Invert
      check = check % 11; // 11 -> 0
      char checksum = (check < 10) ? (char) ('0' + check): 'X';
      return checksum;
    }

    return 0;
  }

  /**
   * @param issnValue ISSN value.
   * @return True if ISSN value is valid.
   */
  public static boolean isValid(String issnValue) {
    if (issnValue == null) {
      return false;
    }
    issnValue = cleanISSN(issnValue);
    if (issnValue.length() != 8) {
      return false;
    }
    if (issnValue.charAt(issnValue.length() - 1) != computeChecksum(issnValue)) {
      return false;
    }
    return true;
  }
}

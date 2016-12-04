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
 * Class containing information about a RFC (Requests for Comments).
 */
public class PageElementRFC extends PageElement {

  /** RFC prefix */
  private final static String RFC_PREFIX = "RFC";

  /** RFC incorrect prefixes */
  private final static String[] RFC_INCORRECT_PREFIX = {
    "rfc"
  };

  /** RFC possible meaningful characters */
  private final static String POSSIBLE_CHARACTERS = "0123456789";

  /** RFC possible extraneous characters */
  private final static String EXTRA_CHARACTERS = "";

  /** RFC incorrect characters */
  private final static String INCORRECT_CHARACTERS = "- :‐\t—=–\n";

  /** RFC incorrect characters at the beginning */
  private final static String INCORRECT_BEGIN_CHARACTERS = "- :‐\t—=–\n";

  /**
   * @param analysis Page analysis.
   * @return List of RFC.
   */
  public static List<PageElementRFC> analyzePage(
      PageAnalysis analysis) {
    List<PageElementRFC> rfcs = new ArrayList<PageElementRFC>();

    // Search for RFC templates
    WPCConfiguration config = analysis.getWPCConfiguration();
    List<String[]> rfcTemplates = config.getStringArrayList(WPCConfigurationStringList.RFC_TEMPLATES);
    if (rfcTemplates != null) {
      for (String[] rfcTemplate : rfcTemplates) {
        if (rfcTemplate.length > 0) {
          List<PageElementTemplate> templates = analysis.getTemplates(rfcTemplate[0]);
          if (templates != null) {
            for (PageElementTemplate template : templates) {
              analyzeTemplateParams(
                  analysis, rfcs, template,
                  (rfcTemplate.length > 1) ? rfcTemplate[1] : "1",
                  false, false, false, false);
            }
          }
        }
      }
    }

    // Search for RFC templates where help is requested
    rfcTemplates = config.getStringArrayList(WPCConfigurationStringList.RFC_HELP_NEEDED_TEMPLATES);
    if (rfcTemplates != null) {
      for (String[] rfcTemplate : rfcTemplates) {
        if (rfcTemplate.length > 0) {
          List<PageElementTemplate> templates = analysis.getTemplates(rfcTemplate[0]);
          if (templates != null) {
            for (PageElementTemplate template : templates) {
              analyzeTemplateParams(
                  analysis, rfcs, template,
                  ((rfcTemplate.length > 1) && (rfcTemplate[1].length() > 0)) ? rfcTemplate[1] : "1",
                  false, false, false, true);
            }
          }
        }
      }
    }

    // Search for RFC in template parameters
    List<PageElementTemplate> templates = analysis.getTemplates();
    for (PageElementTemplate template : templates) {
      analyzeTemplateParams(analysis, rfcs, template, "RFC", true, true, true, false);
    }

    // Search for RFC in plain texts
    analyzePlainText(analysis, rfcs, RFC_PREFIX, true, true);
    for (String prefix : RFC_INCORRECT_PREFIX) {
      analyzePlainText(analysis, rfcs, prefix, false, false);
    }

    return rfcs;
  }

  /**
   * @param index Current index.
   * @param rfcs List of RFC.
   * @return True if the current index is already in a RFC.
   */
  private static boolean isInRFC(int index, List<PageElementRFC> rfcs) {
    if (rfcs != null) {
      for (PageElementRFC tmpRfc : rfcs) {
        if ((tmpRfc.getBeginIndex() <= index) &&
            (tmpRfc.getEndIndex() > index)) {
          return true;
        }
      }
    }
    return false;
  }

  /**
   * Analyze plain text for RFC.
   * 
   * @param analysis Page analysis.
   * @param rfcs Current list of RFC.
   * @param prefix RFC prefix.
   * @param correct True if RFC should be considered correct by default.
   * @param caseSensitive True if RFC prefix is case sensitive.
   */
  private static void analyzePlainText(
      PageAnalysis analysis, List<PageElementRFC> rfcs,
      String prefix, boolean correct, boolean caseSensitive) {
    String contents = analysis.getContents();
    if ((contents == null) || (prefix == null)) {
      return;
    }
    int index = 0;
    int maxIndex = contents.length() - prefix.length();
    while (index < maxIndex) {

      // Check if it's a potential RFC
      boolean isValid = true;
      String nextChars = contents.substring(index, index + prefix.length());
      boolean isRFC = caseSensitive ?
          prefix.equals(nextChars) : prefix.equalsIgnoreCase(nextChars);
      if (isRFC && (analysis.isInComment(index) != null)) {
        isRFC = false;
      }
      if (isRFC && (analysis.isInTag(index) != null)) {
        isRFC = false;
      }
      if (isRFC && (analysis.getSurroundingTag(PageElementTag.TAG_WIKI_NOWIKI, index) != null)) {
        isRFC = false;
      }
      if (isRFC) {
        if ((analysis.getSurroundingTag(PageElementTag.TAG_WIKI_PRE, index) != null) ||
            (analysis.getSurroundingTag(PageElementTag.TAG_WIKI_SOURCE, index) != null) ||
            (analysis.getSurroundingTag(PageElementTag.TAG_WIKI_SYNTAXHIGHLIGHT, index) != null)) {
          isRFC = false;
        }
      }
      if (isRFC && isInRFC(index, rfcs)) {
        isRFC = false;
      }
      if (isRFC) {
        PageElementExternalLink link = analysis.isInExternalLink(index);
        if (link != null) {
          if (!link.hasSquare() ||
              (index < link.getBeginIndex() + link.getTextOffset()) ||
              (link.getText() == null)) {
            isValid = false;
          }
        }
      }
      if (isRFC) {
        PageElementTemplate template = analysis.isInTemplate(index);
        if (template != null) {
          if ((template.getParameterCount() == 0) ||
              (index < template.getParameterPipeIndex(0))) {
            isRFC = false;
          }
        }
      }
      if (isRFC) {
        PageElementImage image = analysis.isInImage(index);
        if (image != null) {
          if (index < image.getBeginIndex() + image.getFirstPipeOffset()) {
            isRFC = false;
          }
        }
      }

      if (isRFC) {

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
            rfcs.add(new PageElementRFC(
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
   * Check if template parameter is a RFC.
   * 
   * @param analysis Page analysis.
   * @param rfcs Current list of RFC.
   * @param template Template.
   * @param argumentName Template parameter name.
   * @param ignoreCase True if parameter name should compared ignoring case.
   * @param acceptNumbers True if numbers are accepted after parameter name.
   * @param acceptAllValues True if all values are accepted, even if not compatible with RFC. 
   * @param helpRequested True if help has been requested for this RFC.
   */
  private static void analyzeTemplateParams(
      PageAnalysis analysis, List<PageElementRFC> rfcs,
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
            rfcs.add(new PageElementRFC(
                beginIndex, endIndex, value, true, correct, helpRequested, true));
          }
        } else if (acceptAllValues) {
          if (paramValue.length() > 0) {
            rfcs.add(new PageElementRFC(
                template.getParameterValueStartIndex(paramNum),
                template.getParameterValueStartIndex(paramNum) + paramValue.length(),
                paramValue, true, false, false, true));
          }
        }
      }
    }
  }

  /**
   * RFC not trimmed.
   */
  private final String rfcNotTrimmed;

  /**
   * RFC (trimmed).
   */
  private final String rfc;

  /**
   * True if RFC is in a valid location.
   */
  private final boolean isValid;

  /**
   * True if RFC syntax is correct.
   */
  private final boolean isCorrect;

  /**
   * True if RFC is a template parameter (RFC=...)
   */
  private final boolean isTemplateParameter;

  /**
   * True if help has been requested for this RFC
   */
  private final boolean helpRequested;

  /**
   * @param beginIndex Begin index.
   * @param endIndex End index.
   * @param rfc RFC.
   * @param isValid True if RFC is in a valid location.
   * @param isCorrect True if RFC syntax is correct.
   * @param helpRequested True if help has been requested for this RFC. 
   * @param isTemplateParameter True if RFC is a template parameter.
   */
  private PageElementRFC(
      int beginIndex, int endIndex,
      String rfc, boolean isValid,
      boolean isCorrect, boolean helpRequested,
      boolean isTemplateParameter) {
    super(beginIndex, endIndex);
    this.rfcNotTrimmed = rfc;
    this.rfc = cleanRFC(rfc);
    this.isValid = isValid;
    this.isCorrect = isCorrect;
    this.helpRequested = helpRequested;
    this.isTemplateParameter = isTemplateParameter;
  }

  /**
   * @return RFC not trimmed.
   */
  public String getRFCNotTrimmed() {
    return rfcNotTrimmed;
  }

  /**
   * @return RFC (trimmed).
   */
  public String getRFC() {
    return rfc;
  }

  /**
   * @return True if RFC is in a valid location.
   */
  public boolean isValid() {
    return isValid;
  }

  /**
   * @return True if RFC syntax is correct.
   */
  public boolean isCorrect() {
    return isCorrect;
  }

  /**
   * @return True if help has been requested for this RFC.
   */
  public boolean helpRequested() {
    return helpRequested;
  }

  /**
   * @return True if RFC is a template parameter.
   */
  public boolean isTemplateParameter() {
    return isTemplateParameter;
  }

  /**
   * @return List of possible RFC.
   */
  public List<String> getCorrectRFC() {
    List<String> result = new ArrayList<String>();
    String prefix = isTemplateParameter ? "" : "RFC ";

    // Construct a basic RFC number
    StringBuilder buffer = new StringBuilder();
    for (int i = 0; i < rfcNotTrimmed.length(); i++) {
      char currentChar = rfcNotTrimmed.charAt(i);
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
    String cleanedRFC = buffer.toString().trim();

    // Basic replacement
    result.add(prefix + cleanedRFC);
    
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
    replacement.append(getRFCNotTrimmed());

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
   * @param rfc RFC number.
   * @return Cleaned up RFC number.
   */
  public static String cleanRFC(String rfc) {
    if (rfc == null) {
      return null;
    }
    rfc = rfc.trim();
    if (rfc.length() == 0) {
      return rfc;
    }
    PageAnalysis analysis = new PageAnalysis(null, rfc);
    StringBuilder result = new StringBuilder();
    int i = 0;
    while (i < rfc.length()) {
      char current = Character.toUpperCase(rfc.charAt(i));
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
   * @param rfcValue RFC value.
   * @return True if RFC value is valid.
   */
  public static boolean isValid(String rfcValue) {
    if (rfcValue == null) {
      return false;
    }
    rfcValue = cleanRFC(rfcValue);
    if (rfcValue.length() == 0) {
      return false;
    }
    return true;
  }
}

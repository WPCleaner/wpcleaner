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
 * Class containing information about an ISBN.
 */
public class PageElementISBN extends PageElement {

  /** ISBN prefix */
  public final static String ISBN_PREFIX = "ISBN";

  /** ISBN incorrect prefixes */
  private final static String[] ISBN_INCORRECT_PREFIX = {
    "ISNB",
    "isbn"
  };

  /** ISBN possible meaningful characters */
  public final static String POSSIBLE_CHARACTERS = "0123456789X";

  /** ISBN possible extraneous characters */
  public final static String EXTRA_CHARACTERS = "- \u00A0";

  /** ISBN incorrect characters */
  private final static String INCORRECT_CHARACTERS = ":‐\t—=–#";

  /** ISBN incorrect characters at the beginning */
  private final static String INCORRECT_BEGIN_CHARACTERS = ":;‐\t—=–#('|.";

  /**
   * @param analysis Page analysis.
   * @return List of ISBN.
   */
  public static List<PageElementISBN> analyzePage(
      PageAnalysis analysis) {
    List<PageElementISBN> isbns = new ArrayList<PageElementISBN>();

    // Configuration
    WPCConfiguration config = analysis.getWPCConfiguration();
    PageElementISBNConfiguration isbnConfig = new PageElementISBNConfiguration(config);
    List<String[]> isbnIgnoreIncorrect = config.getStringArrayList(WPCConfigurationStringList.ISBN_IGNORE_INCORRECT_TEMPLATES);

    // Search for ISBN templates
    List<String[]> isbnTemplates = config.getStringArrayList(WPCConfigurationStringList.ISBN_TEMPLATES);
    if (isbnTemplates != null) {
      for (String[] isbnTemplate : isbnTemplates) {
        if (isbnTemplate.length > 0) {
          String[] params = null;
          List<PageElementTemplate> templates = analysis.getTemplates(isbnTemplate[0]);
          if (templates != null) {
            for (PageElementTemplate template : templates) {
              if (params == null) {
                if (isbnTemplate.length > 1) {
                  params = isbnTemplate[1].split(",");
                } else {
                  params = new String[]{ "1" };
                }
              }
              for (String param : params) {
                if ((param != null) && (param.length() > 0)) {
                  analyzeTemplateParams(
                      analysis, isbns, isbnConfig,
                      template, param,
                      false, false, true, false);
                }
              }
            }
          }
        }
      }
    }

    // Search for ISBN templates where help is requested
    isbnTemplates = config.getStringArrayList(WPCConfigurationStringList.ISBN_HELP_NEEDED_TEMPLATES);
    if (isbnTemplates != null) {
      for (String[] isbnTemplate : isbnTemplates) {
        if (isbnTemplate.length > 0) {
          List<PageElementTemplate> templates = analysis.getTemplates(isbnTemplate[0]);
          if (templates != null) {
            for (PageElementTemplate template : templates) {
              analyzeTemplateParams(
                  analysis, isbns, isbnConfig,
                  template,
                  ((isbnTemplate.length > 1) && (isbnTemplate[1].length() > 0)) ? isbnTemplate[1] : "1",
                  false, false, false, true);
            }
          }
        }
      }
    }

    // Search for ISBN in template parameters
    List<PageElementTemplate> templates = analysis.getTemplates();
    for (PageElementTemplate template : templates) {
      analyzeTemplateParams(
          analysis, isbns, isbnConfig,
          template, "ISBN", true, true, true, false);
    }

    // Search for ISBN in plain texts
    analyzePlainText(analysis, isbns, isbnIgnoreIncorrect);

    return isbns;
  }

  /**
   * @param index Current index.
   * @param isbns List of ISBN.
   * @return True if the current index is already in a ISBN.
   */
  private static boolean isInISBN(int index, List<PageElementISBN> isbns) {
    if (isbns != null) {
      for (PageElementISBN tmpIsbn : isbns) {
        if ((tmpIsbn.getBeginIndex() <= index) &&
            (tmpIsbn.getEndIndex() > index)) {
          return true;
        }
      }
    }
    return false;
  }

  /**
   * Analyze plain text for ISBN.
   * 
   * @param analysis Page analysis.
   * @param isbns Current list of ISBN.
   * @param ignoreIncorrect List of template parameters to ignore when ISBN is incorrect.
   */
  private static void analyzePlainText(
      PageAnalysis analysis, List<PageElementISBN> isbns,
      List<String[]> ignoreIncorrect) {
    String contents = analysis.getContents();
    if (contents == null) {
      return;
    }
    int index = 0;
    int maxIndex = contents.length() - 1;
    while (index < maxIndex) {
      index = checkPlainText(analysis, contents, index, isbns, ignoreIncorrect);
    }
  }

  /**
   * Check plain text for ISBN.
   * 
   * @param analysis Page analysis.
   * @param contents Page contents.
   * @param index Current index in the page.
   * @param isbns Current list of ISBN.
   * @param ignoreIncorrect List of template parameters to ignore when ISBN is incorrect.
   * @return Next index to check.
   */
  private static int checkPlainText(
      PageAnalysis analysis, String contents, int index, List<PageElementISBN> isbns,
      List<String[]> ignoreIncorrect) {

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

    // Check if it's a potential ISBN
    String prefix = null;
    boolean correct = false;
    if (contents.startsWith(ISBN_PREFIX, index)) {
      prefix = ISBN_PREFIX;
      correct = true;
    }
    for (String tmpPrefix : ISBN_INCORRECT_PREFIX) {
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
    if (isInISBN(index, isbns)) {
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
    Parameter parameter = null;
    if (template != null) {
      parameter = template.getParameterAtIndex(index);
    }

    int beginIndex = index;
    index += prefix.length();
    boolean isCorrect = correct;
    if ((parameter == null) ||
        (parameter.getValueStartIndex() < beginIndex)) {
      if (beginIndex >= 2) {
        if (contents.startsWith("[[", beginIndex - 2)) {
          isCorrect = false;
          beginIndex -= 2;
          if ((index + 2 < contents.length()) && contents.startsWith("]]", index)) {
            index += 2;
          }
        }
      }
      if (beginIndex >= 3) {
        if (contents.startsWith("10-", beginIndex - 3) ||
            contents.startsWith("13-", beginIndex - 3)) {
          isCorrect = false;
          beginIndex -= 3;
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

        // Ignore special parameters
        boolean done = false;
        if ((template != null) &&
            (parameter != null) &&
            (isCorrect == false) &&
            (ignoreIncorrect != null)) {
          for (String[] ignore : ignoreIncorrect) {
            if ((ignore.length > 1) &&
                (Page.areSameTitle(ignore[0], template.getTemplateName())) &&
                (ignore[1].equals(parameter.getComputedName()))) {
              done = true;
            }
          }
        }

        // Default definition
        if (!done) {
          isbns.add(new PageElementISBN(
              beginIndex, endNumber, analysis, number,
              isValid, isCorrect, false, null));
        }
        index = endNumber;
      } else {
        if (contents.startsWith(prefix, index) &&
            !contents.startsWith("[[ISBN#", beginIndex)) {
          isbns.add(new PageElementISBN(
              beginIndex, index, analysis, "",
              isValid, false, false, null));
        }
      }
    }

    return index;
  }

  /**
   * Check if template parameter is an ISBN.
   * 
   * @param analysis Page analysis.
   * @param isbns Current list of ISBN.
   * @param isbnConfig Configuration for ISBN.
   * @param template Template.
   * @param argumentName Template parameter name.
   * @param ignoreCase True if parameter name should compared ignoring case.
   * @param acceptNumbers True if numbers are accepted after parameter name.
   * @param acceptAllValues True if all values are accepted, even if not compatible with ISBN. 
   * @param helpRequested True if help has been requested for this ISBN.
   */
  private static void analyzeTemplateParams(
      PageAnalysis analysis, List<PageElementISBN> isbns,
      PageElementISBNConfiguration isbnConfig,
      PageElementTemplate template,
      String argumentName,
      boolean ignoreCase, boolean acceptNumbers,
      boolean acceptAllValues, boolean helpRequested) {

    // Check if template should be ignored
    if (isbnConfig.shouldIgnoreTemplate(template)) {
      return;
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

      // Parameter is for an ISBN, analyze that it's not filled by WikiData
      if (nameOk) {
        String paramValue = param.getValue();
        if ((paramValue == null) ||
            (paramValue.trim().length() == 0) ||
            "{{#property:p212}}".equalsIgnoreCase(paramValue.trim()) || // ISBN-13
            "{{#property:p957}}".equalsIgnoreCase(paramValue.trim())) { // ISBN-10
          nameOk = false;
        }
      }

      // Parameter is for an ISBN, analyze its value
      if (nameOk) {
        String paramValue = param.getStrippedValue();
        boolean ok = true;
        boolean hasDigit = false;
        int i = 0;
        boolean correct = true;
        while (ok && (i < paramValue.length())) {
          char currentChar = paramValue.charAt(i);
          if (POSSIBLE_CHARACTERS.indexOf(currentChar) >= 0) {
            if (Character.isDigit(currentChar)) {
              hasDigit = true;
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
        int beginIndex = param.getValueStartIndex();
        int endIndex = (paramNum + 1 < template.getParameterCount()) ?
            template.getParameterPipeIndex(paramNum + 1) :
            template.getEndIndex() - 2;
        while ((endIndex > beginIndex) &&
            Character.isWhitespace(analysis.getContents().charAt(endIndex - 1))) {
          endIndex--;
        }
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
          String contents = analysis.getContents();
          String value = contents.substring(beginIndex, endIndex);

          // Detect prefixes "10-" and "13-" before templates
          if (correct) {
            int tmpIndex = template.getBeginIndex();
            while ((tmpIndex > 0) && (contents.charAt(tmpIndex - 1) == ' ')) {
              tmpIndex--;
            }
            if ((tmpIndex > 0) && (contents.charAt(tmpIndex - 1) == '-')) {
              tmpIndex--;
              while ((tmpIndex > 0) && (contents.charAt(tmpIndex - 1) == ' ')) {
                tmpIndex--;
              }
              if (tmpIndex >= 2) {
                if (contents.startsWith("10", tmpIndex - 2) ||
                    contents.startsWith("13", tmpIndex - 2)) {
                  if ((tmpIndex == 2) || !Character.isDigit(contents.charAt(tmpIndex - 3))) {
                    correct = false;
                    beginIndex = tmpIndex - 2;
                    endIndex = template.getEndIndex();
                  }
                }
              }
            }
          }

          if (paramValue.length() > 0) {
            isbns.add(new PageElementISBN(
                beginIndex, endIndex, analysis, value,
                true, correct, helpRequested, template));
          }
        } else if (acceptAllValues) {
          if (paramValue.length() > 0) {
            isbns.add(new PageElementISBN(
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

  /** ISBN not trimmed */
  private final String isbnNotTrimmed;

  /** ISBN (trimmed) */
  private final String isbn;

  /** True if ISBN is in a valid location */
  private final boolean isValid;

  /** True if ISBN syntax is correct */
  private final boolean isCorrect;

  /** Template if ISBN is a template parameter (ISBN=...) */
  private final PageElementTemplate template;

  /** True if help has been requested for this ISBN */
  private final boolean helpRequested;

  /**
   * @param beginIndex Begin index.
   * @param endIndex End index.
   * @param analysis Page analysis.
   * @param isbn ISBN.
   * @param isValid True if ISBN is in a valid location.
   * @param isCorrect True if ISBN syntax is correct.
   * @param helpRequested True if help has been requested for this ISBN. 
   * @param template Template if ISBN is a template parameter.
   */
  private PageElementISBN(
      int beginIndex, int endIndex, PageAnalysis analysis,
      String isbn, boolean isValid,
      boolean isCorrect, boolean helpRequested,
      PageElementTemplate template) {
    super(beginIndex, endIndex);
    this.wpcConfiguration = analysis.getWPCConfiguration();
    this.fullText = analysis.getContents().substring(beginIndex, endIndex);
    this.isbnNotTrimmed = isbn;
    this.isbn = cleanISBN(isbn);
    this.isValid = isValid;
    this.isCorrect = isCorrect;
    this.helpRequested = helpRequested;
    this.template = template;
  }

  /**
   * @return ISBN not trimmed.
   */
  public String getISBNNotTrimmed() {
    return isbnNotTrimmed;
  }

  /**
   * @return ISBN (trimmed).
   */
  public String getISBN() {
    return isbn;
  }

  /**
   * @return True if ISBN is in a valid location.
   */
  public boolean isValid() {
    return isValid;
  }

  /**
   * @return True if ISBN syntax is correct.
   */
  public boolean isCorrect() {
    return isCorrect;
  }

  /**
   * @return True if help has been requested for this ISBN.
   */
  public boolean helpRequested() {
    return helpRequested;
  }

  /**
   * @return True if ISBN is a template parameter.
   */
  public boolean isTemplateParameter() {
    return (template != null);
  }

  /**
   * @return List of possible ISBN.
   */
  public List<String> getCorrectISBN() {
    List<String> result = new ArrayList<String>();
    String prefix = isTemplateParameter() ? "" : "ISBN ";

    // Prefix outside the template
    if ((template != null) &&
        (getBeginIndex() < template.getBeginIndex())) {
      if (fullText != null) {
        result.add(fullText.substring(template.getBeginIndex() - getBeginIndex()));
      }
      return result;
    }

    // Construct a basic ISBN number
    StringBuilder buffer = new StringBuilder();
    for (int i = 0; i < isbnNotTrimmed.length(); i++) {
      char currentChar = isbnNotTrimmed.charAt(i);
      if ((POSSIBLE_CHARACTERS.indexOf(currentChar) >= 0) ||
          (EXTRA_CHARACTERS.indexOf(currentChar) >= 0)) {
        buffer.append(currentChar);
      } else if ((currentChar == '‐') ||
                 (currentChar == '–') ||
                 (currentChar == '.')) {
        buffer.append("-");
      } else if (currentChar == '\t') {
        buffer.append(" ");
      } else if (currentChar == 'x') {
        buffer.append("X");
      } else {
        buffer.append(currentChar);
      }
    }
    String cleanedISBN = buffer.toString().trim();

    // ISBN-10
    if ((isbn.length() == 12) && (isbn.startsWith("10"))) {
      boolean ok = true;
      int index = 0;
      while ((index < cleanedISBN.length()) &&
             ((Character.isWhitespace(cleanedISBN.charAt(index))) ||
              (cleanedISBN.charAt(index) == '-'))) {
        index++;
      }
      if ((index < cleanedISBN.length() && (cleanedISBN.charAt(index) == '1'))) {
        index++;
      } else {
        ok = false;
      }
      while ((index < cleanedISBN.length()) &&
             ((Character.isWhitespace(cleanedISBN.charAt(index))) ||
              (cleanedISBN.charAt(index) == '-'))) {
        index++;
      }
      if ((index < cleanedISBN.length() && (cleanedISBN.charAt(index) == '0'))) {
        index++;
      } else {
        ok = false;
      }
      while ((index < cleanedISBN.length()) &&
             ((!Character.isDigit(cleanedISBN.charAt(index))) &&
              (cleanedISBN.charAt(index) != 'X'))) {
        index++;
      }
      if (ok && (index < cleanedISBN.length())) {
        addCorrectISBN(result, prefix, cleanedISBN.substring(index));
      }
    }

    // ISBN-13
    if ((isbn.length() == 15) && (isbn.startsWith("13"))) {
      boolean ok = true;
      int index = 0;
      while ((index < cleanedISBN.length()) &&
             ((Character.isWhitespace(cleanedISBN.charAt(index))) ||
              (cleanedISBN.charAt(index) == '-'))) {
        index++;
      }
      if ((index < cleanedISBN.length() && (cleanedISBN.charAt(index) == '1'))) {
        index++;
      } else {
        ok = false;
      }
      while ((index < cleanedISBN.length()) &&
             ((Character.isWhitespace(cleanedISBN.charAt(index))) ||
              (cleanedISBN.charAt(index) == '-'))) {
        index++;
      }
      if ((index < cleanedISBN.length() && (cleanedISBN.charAt(index) == '3'))) {
        index++;
      } else {
        ok = false;
      }
      while ((index < cleanedISBN.length()) &&
             ((!Character.isDigit(cleanedISBN.charAt(index))) &&
              (cleanedISBN.charAt(index) != 'X'))) {
        index++;
      }
      if (ok && (index < cleanedISBN.length())) {
        addCorrectISBN(result, prefix, cleanedISBN.substring(index));
      }
    }

    // Basic replacement
    addCorrectISBN(result, prefix, cleanedISBN);

    // Common mistyped characters
    cleanedISBN = cleanedISBN.replaceAll("x", "X");
    cleanedISBN = cleanedISBN.replaceAll("O", "0");
    cleanedISBN = cleanedISBN.replaceAll("I", "1");
    cleanedISBN = cleanedISBN.replaceAll("B", "8");
    addCorrectISBN(result, prefix, cleanedISBN);

    return result;
  }

  /**
   * @param result List of possible replacements.
   * @param prefix ISBN prefix.
   * @param cleanedISBN Cleaned up ISBN.
   */
  private void addCorrectISBN(List<String> result, String prefix, String cleanedISBN) {
    addCorrectISBN(result, prefix + cleanedISBN);
    if (!isTemplateParameter()) {
      List<String[]> isbnTemplates = wpcConfiguration.getStringArrayList(
          WPCConfigurationStringList.ISBN_TEMPLATES);
      if (isbnTemplates != null) {
        for (String[] isbnTemplate : isbnTemplates) {
          if (isbnTemplate.length > 2) {
            String[] params = isbnTemplate[1].split(",");
            Boolean suggested = Boolean.valueOf(isbnTemplate[2]);
            if ((params.length > 0) && (Boolean.TRUE.equals(suggested))) {
              StringBuilder buffer = new StringBuilder();
              buffer.append("{{");
              buffer.append(isbnTemplate[0]);
              buffer.append("|");
              if (!"1".equals(params[0])) {
                buffer.append(params[0]);
                buffer.append("=");
              }
              buffer.append(cleanedISBN);
              buffer.append("}}");
              addCorrectISBN(result, buffer.toString());
            }
          }
        }
      }
      
    }
  }

  /**
   * @param result List of possible replacements.
   * @param correctISBN Possible replacement.
   */
  private void addCorrectISBN(List<String> result, String correctISBN) {
    if ((result == null) || (correctISBN == null)) {
      return;
    }
    if (!result.contains(correctISBN)) {
      result.add(correctISBN);
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

    // ISBN
    replacement.append("|");
    if ((helpNeededTemplate.length > 1) &&
        (helpNeededTemplate[1].length() > 0)) {
      replacement.append(helpNeededTemplate[1]);
      replacement.append("=");
    }
    replacement.append(getISBNNotTrimmed());

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
   * @param isbn ISBN number.
   * @return Cleaned up ISBN number.
   */
  public static String cleanISBN(String isbn) {
    if (isbn == null) {
      return null;
    }
    isbn = isbn.trim();
    if (isbn.length() == 0) {
      return isbn;
    }
    PageAnalysis analysis = new PageAnalysis(null, isbn);
    StringBuilder result = new StringBuilder();
    int i = 0;
    while (i < isbn.length()) {
      char current = Character.toUpperCase(isbn.charAt(i));
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
   * @param isbnValue ISBN value.
   * @return Computed checksum.
   */
  public static char computeChecksum(String isbnValue) {
    if (isbnValue == null) {
      return 0;
    }
    isbnValue = cleanISBN(isbnValue);

    // Check for ISBN-10
    if (isbnValue.length() == 10) {
      int check = 0;
      for (int i = 0; i < 9; i++) {
        char currentChar = isbnValue.charAt(i);
        if (Character.isDigit(currentChar)) {
          check += (10 - i) * (currentChar - '0');
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

    // Check for ISBN-13
    if (isbnValue.length() == 13) {
      int check = 0;
      for (int i = 0; i < 12; i++) {
        char currentChar = isbnValue.charAt(i);
        if (Character.isDigit(currentChar)) {
          check += ((i % 2 == 0) ? 1 : 3) * (currentChar - '0');
        } else {
          return 0;
        }
      }
      check = check % 10; // Modulus 10
      check = 10 - check; // Invert
      check = check % 10; // 10 -> 0
      char checksum = (char) ('0' + check);
      return checksum;
    }

    return 0;
  }

  /**
   * @param isbnValue ISBN value.
   * @return True if ISBN value is valid.
   */
  public static boolean isValid(String isbnValue) {
    if (isbnValue == null) {
      return false;
    }
    isbnValue = cleanISBN(isbnValue);
    if ((isbnValue.length() != 10) && (isbnValue.length() != 13)) {
      return false;
    }
    if (isbnValue.charAt(isbnValue.length() - 1) != computeChecksum(isbnValue)) {
      return false;
    }
    return true;
  }
}

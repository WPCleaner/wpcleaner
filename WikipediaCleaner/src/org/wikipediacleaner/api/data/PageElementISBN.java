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


/**
 * Class containing information about an ISBN.
 */
public class PageElementISBN extends PageElement {

  /**
   * ISBN prefix.
   */
  private final static String ISBN_PREFIX = "ISBN";

  /**
   * ISBN possible meaningful characters.
   */
  private final static String POSSIBLE_CHARACTERS = "0123456789Xx";

  /**
   * ISBN possible extraneous characters.
   */
  private final static String EXTRA_CHARACTERS = "-  ";

  /**
   * ISBN incorrect characters.
   */
  private final static String INCORRECT_CHARACTERS = ":‐\t—=–";

  /**
   * @param analysis Page analysis.
   * @return List of ISBN.
   */
  public static List<PageElementISBN> analyzePage(
      PageAnalysis analysis) {
    List<PageElementISBN> isbns = new ArrayList<PageElementISBN>();

    // Search for ISBN in plain texts
    String contents = analysis.getContents();
    int index = 0;
    int maxIndex = contents.length() - ISBN_PREFIX.length();
    while (index < maxIndex) {

      // Check if it's a potential ISBN
      boolean isValid = true;
      boolean isISBN = ISBN_PREFIX.equalsIgnoreCase(
          contents.substring(index, index + ISBN_PREFIX.length()));
      if (isISBN && (analysis.isInComment(index) != null)) {
        isISBN = false;
      }
      if (isISBN && (analysis.isInTag(index) != null)) {
        isISBN = false;
      }
      if (isISBN) {
        PageElementExternalLink link = analysis.isInExternalLink(index);
        if (link != null) {
          if (!link.hasSquare() ||
              (index < link.getBeginIndex() + link.getTextOffset()) ||
              (link.getText() == null)) {
            isValid = false;
          }
        }
      }

      if (isISBN) {

        // Check if it's a template parameter
        boolean parameter = false;
        PageElementTemplate template = analysis.isInTemplate(index);
        if (template != null) {
          for (int paramNum = 0; paramNum < template.getParameterCount(); paramNum++) {
            if ((template.getParameterPipeOffset(paramNum) < index) &&
                (template.getParameterValueOffset(paramNum) > index)) {
              parameter = true;
            }
          }
        }

        int beginIndex = index;
        index += ISBN_PREFIX.length();
        if (!parameter) {
          boolean spaceFound = false;
          if (analysis.isInComment(index) == null) {
            while ((index < contents.length()) && (contents.charAt(index) == ' ')) {
              index++;
              spaceFound = true;
            }
          }
          int beginNumber = -1;
          int endNumber = beginNumber;
          boolean finished = false;
          boolean correct = spaceFound;
          boolean nextCorrect = correct;
          while (!finished && (index < contents.length())) {
            char currentChar = contents.charAt(index);
            if (POSSIBLE_CHARACTERS.indexOf(currentChar) >= 0) {
              if (beginNumber < 0) {
                beginNumber = index;
              }
              endNumber = index + 1;
              index++;
              correct = nextCorrect;
            } else if (EXTRA_CHARACTERS.indexOf(currentChar) >= 0) {
              if (beginNumber < 0) {
                nextCorrect = false;
              }
              index++;
            } else if (INCORRECT_CHARACTERS.indexOf(currentChar) >= 0) {
              index++;
              nextCorrect = false;
            } else {
              finished = true;
            }
          }
          if (endNumber > beginNumber) {
            String number = contents.substring(beginNumber, endNumber);
            isbns.add(new PageElementISBN(
                beginIndex, endNumber, number, isValid, correct, false));
            index = endNumber;
          }
        }
      } else {
        index++;
      }
    }

    // Search for ISBN templates
    WPCConfiguration config = analysis.getWPCConfiguration();
    List<String[]> isbnTemplates = config.getStringArrayList(WPCConfigurationStringList.ISBN_TEMPLATES);
    if (isbnTemplates != null) {
      for (String[] isbnTemplate : isbnTemplates) {
        if (isbnTemplate.length > 0) {
          List<PageElementTemplate> templates = analysis.getTemplates(isbnTemplate[0]);
          if (templates != null) {
            for (PageElementTemplate template : templates) {
              analyzeTemplateParams(
                  analysis, isbns, template,
                  (isbnTemplate.length > 1) ? isbnTemplate[1] : "1",
                  false, false);
            }
          }
        }
      }
    }

    // Search for ISBN in template parameters
    List<PageElementTemplate> templates = analysis.getTemplates();
    for (PageElementTemplate template : templates) {
      analyzeTemplateParams(analysis, isbns, template, "ISBN", true, true);
    }

    return isbns;
  }

  /**
   * Check if template parameter is an ISBN.
   * 
   * @param analysis Page analysis.
   * @param isbns Current list of ISBN.
   * @param template Template.
   * @param argumentName Template parameter name.
   * @param ignoreCase True if parameter name should compared ignoring case.
   * @param acceptNumbers True if numbers are accepted after parameter name.
   */
  private static void analyzeTemplateParams(
      PageAnalysis analysis, List<PageElementISBN> isbns,
      PageElementTemplate template,
      String argumentName,
      boolean ignoreCase, boolean acceptNumbers) {
    int paramDefaultName = 1;
    for (int paramNum = 0; paramNum < template.getParameterCount(); paramNum++) {

      // Check parameter name
      String paramName = template.getParameterName(paramNum);
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
        String paramValue = template.getParameterValue(paramNum);
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
        int delta = template.getParameterValueOffset(paramNum);
        beginIndex += delta;
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
            isbns.add(new PageElementISBN(
                beginIndex, endIndex, value, true, correct, true));
          }
        }
      }
    }
  }

  /**
   * ISBN not trimmed.
   */
  private String isbnNotTrimmed;

  /**
   * ISBN (trimmed).
   */
  private String isbn;

  /**
   * True if ISBN is in a valid location.
   */
  private boolean isValid;

  /**
   * True if ISBN syntax is correct.
   */
  private boolean isCorrect;

  /**
   * True if ISBN is a template parameter (ISBN=...)
   */
  private boolean isTemplateParameter;

  /**
   * @param beginIndex Begin index.
   * @param endIndex End index.
   * @param isbn ISBN.
   * @param isValid True if ISBN is in a valid location.
   * @param isCorrect True if ISBN syntax is correct.
   * @param isTemplateParameter True if ISBN is a template parameter.
   */
  private PageElementISBN(
      int beginIndex, int endIndex,
      String isbn, boolean isValid,
      boolean isCorrect, boolean isTemplateParameter) {
    super(beginIndex, endIndex);
    this.isbnNotTrimmed = isbn;
    this.isbn = cleanISBN(isbn);
    this.isValid = isValid;
    this.isCorrect = isCorrect;
    this.isTemplateParameter = isTemplateParameter;
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
   * @return Check.
   */
  public char getCheck() {
    if (isbn == null) {
      return 0;
    }

    // Check for ISBN-10
    if (isbn.length() == 10) {
      int check = 0;
      for (int i = 0; i < 9; i++) {
        char currentChar = isbn.charAt(i);
        if (Character.isDigit(currentChar)) {
          check += (10 - i) * (currentChar - '0');
        } else {
          return 0;
        }
      }
      check = check % 11; // Modulus 11
      check = 11 - check; // Invert
      check = check % 11; // 11 -> 0
      char computedCheck = (check < 10) ? (char) ('0' + check): 'X';
      return computedCheck;
    }

    // Check for ISBN-13
    if (isbn.length() == 13) {
      int check = 0;
      for (int i = 0; i < 12; i++) {
        char currentChar = isbn.charAt(i);
        if (Character.isDigit(currentChar)) {
          check += ((i % 2 == 0) ? 1 : 3) * (currentChar - '0');
        } else {
          return 0;
        }
      }
      check = check % 10; // Modulus 10
      check = 10 - check; // Invert
      check = check % 10; // 10 -> 0
      char computedCheck = (char) ('0' + check);
      return computedCheck;
    }

    return 0;
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
   * @return True if ISBN is a template parameter.
   */
  public boolean isTemplateParameter() {
    return isTemplateParameter;
  }

  /**
   * @return Fixed ISBN.
   */
  public List<String> getCorrectISBN() {
    List<String> result = new ArrayList<String>();
    String prefix = isTemplateParameter ? "" : "ISBN ";

    // Construct a basic ISBN number
    StringBuilder buffer = new StringBuilder();
    for (int i = 0; i < isbnNotTrimmed.length(); i++) {
      char currentChar = isbnNotTrimmed.charAt(i);
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
        result.add(prefix + cleanedISBN.substring(index));
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
        result.add(prefix + cleanedISBN.substring(index));
      }
    }

    // Basic replacement
    result.add(prefix + cleanedISBN);
    
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
  private static String cleanISBN(String isbn) {
    if (isbn == null) {
      return null;
    }
    isbn = isbn.trim();
    if (isbn.length() == 0) {
      return isbn;
    }
    StringBuilder result = new StringBuilder();
    for (int i = 0; i < isbn.length(); i++) {
      char current = Character.toUpperCase(isbn.charAt(i));
      if (((current >= '0') && (current <= '9')) || (current == 'X')) {
        result.append(current);
      }
    }
    return result.toString();
  }
}

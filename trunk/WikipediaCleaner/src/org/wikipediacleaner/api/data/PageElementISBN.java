/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.api.data;

import java.util.ArrayList;
import java.util.List;


/**
 * Class containing information about an ISBN.
 */
public class PageElementISBN extends PageElement {

  /**
   * ISBN prefix.
   */
  private final static String ISBN_PREFIX = "ISBN";

  /**
   * ISBN possible characters.
   */
  private final static String POSSIBLE_CHARACTERS = "0123456789Xx- ";

  /**
   * @param analysis Page analysis.
   * @return List of ISBN.
   */
  public static List<PageElementISBN> analyzePage(
      PageAnalysis analysis) {
    List<PageElementISBN> isbns = new ArrayList<PageElementISBN>();

    // Search for ISBN in plain texts
    String contents = analysis.getContents();
    int index = contents.indexOf(ISBN_PREFIX);
    while (index >= 0) {
      int beginIndex = index;
      index += ISBN_PREFIX.length();
      boolean spaceFound = false;
      while ((index < contents.length()) && (contents.charAt(index) == ' ')) {
        index++;
        spaceFound = true;
      }
      if (spaceFound) {
        int beginNumber = index;
        int endIndex = beginNumber;
        while ((index < contents.length()) &&
               (POSSIBLE_CHARACTERS.indexOf(contents.charAt(index)) >= 0)) {
          if ((contents.charAt(index) != ' ') &&
              (contents.charAt(index) != '-')) {
            endIndex = index + 1;
          }
          index++;
        }
        if (endIndex > beginNumber) {
          String number = contents.substring(beginNumber, endIndex);
          isbns.add(new PageElementISBN(
              beginIndex, endIndex, number, false));
        }
      }
      index = contents.indexOf(ISBN_PREFIX, index);
    }

    // Search for ISBN in template parameters
    List<PageElementTemplate> templates = analysis.getTemplates();
    for (PageElementTemplate template : templates) {
      for (int paramNum = 0; paramNum < template.getParameterCount(); paramNum++) {
        String paramName = template.getParameterName(paramNum);
        if ("ISBN".equalsIgnoreCase(paramName)) {
          String paramValue = template.getParameterValue(paramNum);
          boolean ok = true;
          for (int i = 0; i < paramValue.length(); i++) {
            if (POSSIBLE_CHARACTERS.indexOf(paramValue.charAt(i)) < 0) {
              ok = false;
            }
          }
          if (ok) {
            paramValue = paramValue.trim();
            if (paramValue.length() > 0) {
              int beginIndex = template.getParameterValueOffset(paramNum);
              int endIndex = (paramNum + 1 < template.getParameterCount()) ?
                  template.getParameterPipeOffset(paramNum + 1) :
                  template.getEndIndex() - 2;
              isbns.add(new PageElementISBN(
                  beginIndex, endIndex, paramValue, true));
            }
          }
        }
      }
    }

    return isbns;
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
   * True if ISBN is a template parameter (ISBN=...)
   */
  private boolean isTemplateParameter;

  /**
   * @param beginIndex Begin index.
   * @param endIndex End index.
   */
  private PageElementISBN(
      int beginIndex, int endIndex,
      String isbn, boolean isTemplateParameter) {
    super(beginIndex, endIndex);
    this.isbnNotTrimmed = isbn;
    this.isbn = cleanISBN(isbn);
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
        }
      }
      check = check % 11; // Modulus 11
      check = 11 - check; // Invert
      check = check % 11; // 11 -> 0
      char computedCheck = (check < 10) ? (char) ('0' + check): 'X';
      return computedCheck;
    }

    return 0;
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

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
               ("0123456789X- ".indexOf(contents.charAt(index)) >= 0)) {
          if (contents.charAt(index) != ' ') {
            endIndex = index + 1;
          }
          index++;
        }
        if (endIndex > beginNumber) {
          String number = contents.substring(beginNumber, endIndex);
          isbns.add(new PageElementISBN(
              beginIndex, endIndex, number));
        }
      }
      index = contents.indexOf(ISBN_PREFIX, index);
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
   * @param beginIndex Begin index.
   * @param endIndex End index.
   */
  private PageElementISBN(
      int beginIndex, int endIndex,
      String isbn) {
    super(beginIndex, endIndex);
    this.isbnNotTrimmed = isbn;
    this.isbn = cleanISBN(isbn);
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
      char current = isbn.charAt(i);
      if (((current >= '0') && (current <= '9')) || (current == 'X')) {
        result.append(current);
      }
    }
    return result.toString();
  }
}

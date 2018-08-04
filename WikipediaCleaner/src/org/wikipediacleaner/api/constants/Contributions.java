/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.constants;

import org.wikipediacleaner.i18n.GT;


/**
 * Keep record of contributions done by the user.
 */
public class Contributions {

  /**
   * Wikipedia on which contributions have been made.
   */
  private final EnumWikipedia wikipedia;

  /**
   * @param wikipedia Wikipedia.
   */
  public Contributions(EnumWikipedia wikipedia) {
    this.wikipedia = wikipedia;
  }

  /* ========================================================================= */
  /* General information                                                       */
  /* ========================================================================= */

  /**
   * @return Descriptive text of the contributions.
   */
  public String getDescription() {

    // Check if contributions have been made.
    boolean contributions = false;
    StringBuilder buffer = new StringBuilder();
    buffer.append(GT._T("You have done the following contributions on \"{0}\":", wikipedia.toString()));
    buffer.append("\n<br/>");
    if (pages > 0) {
      contributions = true;
      buffer.append("    → ");
      buffer.append(GT.__(
          "You have edited {0} page.",
          "You have edited {0} pages.",
          pages, Integer.toString(pages)));
      buffer.append("\n<br/>");
    }
    if (dabLinks > 0) {
      contributions = true;
      buffer.append("    → ");
      buffer.append(GT.__(
          "You have fixed {0} link to disambiguation pages.",
          "You have fixed {0} links to disambiguation pages.",
          dabLinks, Integer.toString(dabLinks)));
      buffer.append("\n<br/>");
    }
    for (int errorNumber = 0; errorNumber < checkWikiErrors.length; errorNumber++) {
      if (checkWikiErrors[errorNumber] > 0) {
        contributions = true;
        CWConfiguration configuration = wikipedia.getCWConfiguration();
        CWConfigurationError errorConfiguration = configuration.getErrorConfiguration(errorNumber);
        buffer.append("    → ");
        buffer.append(GT.__(
            "You have fixed {0} error \"{1}\".",
            "You have fixed {0} errors \"{1}\".",
            checkWikiErrors[errorNumber],
            new Object[] {
                Integer.toString(checkWikiErrors[errorNumber]),
                errorConfiguration.getShortDescription() } ));
        buffer.append("\n<br/>");
      }
    }
    if (contributions) {
      return buffer.toString();
    }

    return GT._T("You haven''t done any contribution on \"{0}\"", wikipedia.toString());
  }

  /**
   * Take into account last contributions.
   * 
   * @param other Last contributions.
   */
  synchronized public void increaseContributions(Contributions other) {
    if (other == null) {
      return;
    }
    pages += other.pages;
    dabLinks += other.dabLinks;
    for (int errorNumber = 0; errorNumber < checkWikiErrors.length; errorNumber++) {
      checkWikiErrors[errorNumber] += other.checkWikiErrors[errorNumber];
    }
  }

  /* ========================================================================= */
  /* Count of edited pages                                                     */
  /* ========================================================================= */

  /**
   * Number of edited pages.
   */
  private int pages;

  /**
   * @param count Count of pages that have been edited.
   */
  public void increasePages(int count) {
    if (count > 0) {
      pages += count;
    }
  }

  /* ========================================================================= */
  /* Count of links to disambiguation pages that have been fixed               */
  /* ========================================================================= */

  /**
   * Number of links to disambiguation pages.
   */
  private int dabLinks;

  /**
   * @param count Count of links that have been fixed.
   */
  public void increaseDabLinks(int count) {
    if (count > 0) {
      dabLinks += count;
    }
  }

  /* ========================================================================= */
  /* Count of Check Wiki errors that have been fixed                           */
  /* ========================================================================= */

  /**
   * Number of Check Wiki errors.
   */
  private int[] checkWikiErrors = new int[CWConfiguration.MAX_ERROR_NUMBER];

  /**
   * @param errorNumber Check Wiki error number.
   * @param count Count of errors that have been fixed.
   */
  public void increaseCheckWikiError(int errorNumber, int count) {
    if ((errorNumber >= 0) &&
        (errorNumber < checkWikiErrors.length) &&
        (count > 0)) {
      checkWikiErrors[errorNumber] += count;
    }
  }
}

/*
 *  WikipediaCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2012  Nicolas Vervelle
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
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
    buffer.append(GT._("You have done the following contributions on \"{0}\":", wikipedia.toString()));
    buffer.append("\n");
    if (pages > 0) {
      contributions = true;
      buffer.append("    → ");
      buffer.append(GT._("You have edited {0} page(s).", Integer.toString(pages)));
      buffer.append("\n");
    }
    if (dabLinks > 0) {
      contributions = true;
      buffer.append("    → ");
      buffer.append(GT._("You have fixed {0} link(s) to disambiguation pages.", Integer.toString(dabLinks)));
      buffer.append("\n");
    }
    for (int errorNumber = 0; errorNumber < checkWikiErrors.length; errorNumber++) {
      if (checkWikiErrors[errorNumber] > 0) {
        contributions = true;
        CWConfiguration configuration = wikipedia.getCWConfiguration();
        CWConfigurationError errorConfiguration = configuration.getErrorConfiguration(errorNumber);
        buffer.append("    → ");
        buffer.append(GT._(
            "You have fixed {0} error(s) \"{1}\".",
            new Object[] {
                Integer.toString(checkWikiErrors[errorNumber]),
                errorConfiguration.getShortDescriptionReplaced() } ));
        buffer.append("\n");
      }
    }
    if (contributions) {
      return buffer.toString();
    }

    return GT._("You haven''t done any contribution on \"{0}\"", wikipedia.toString());
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

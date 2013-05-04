/*
 *  WikipediaCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2008  Nicolas Vervelle
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

package org.wikipediacleaner.api.data;

import java.util.Collection;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.regex.PatternSyntaxException;


/**
 * Utility class to memorize automatic fixing parameters
 */
public class AutomaticFixing implements Comparable<AutomaticFixing> {

  /**
   * Original text.
   */
  private String originalText;

  /**
   * Replacement text.
   */
  private String replacementText;

  /**
   * Regular expression or not.
   */
  private boolean regex;

  /**
   * Default constructor.
   */
  public AutomaticFixing() {
    //
  }

  /**
   * Constructor.
   * 
   * @param from Original text.
   * @param to Replacement text.
   * @param regex Regular expression or not.
   */
  public AutomaticFixing(String from, String to, boolean regex) {
    originalText = from;
    replacementText = to;
    this.regex = regex;
  }

  /**
   * @return Original text.
   */
  public String getOriginalText() {
    return originalText;
  }

  /**
   * @param text Original text.
   */
  public void setOriginalText(String text) {
    originalText = text;
  }

  /**
   * @return Replacement text.
   */
  public String getReplacementText() {
    return replacementText;
  }

  /**
   * @param text Replacement text.
   */
  public void setReplacementText(String text) {
    replacementText = text;
  }

  /**
   * @return Regular expression or not.
   */
  public Boolean getRegex() {
    return Boolean.valueOf(regex);
  }

  /**
   * @param flag Regular expression or not.
   */
  public void setRegex(Boolean flag) {
    regex = Boolean.TRUE.equals(flag);
  }

  /**
   * Apply a list of automatic fixing expressions to a text.
   * 
   * @param fixing List of automatic fixing expressions.
   * @param text Original text.
   * @param replacements Optional list of replacements performed.
   * @return Text with replacements done.
   */
  public static String apply(
      Collection<AutomaticFixing> fixing, String text,
      List<String> replacements) {
    if ((text == null) || (fixing == null) || (fixing.isEmpty())) {
      return text;
    }

    // Apply each automatic fixing expression to the text
    StringBuffer tmpText = new StringBuffer();
    for (AutomaticFixing replacement : fixing) {

      // Initialize data
      tmpText.setLength(0);
      String originalText = replacement.getOriginalText();
      String replacementText = replacement.getReplacementText();
      if (replacementText == null) {
        replacementText = "";
      }

      int currentIndex = 0;
      if (replacement.regex) {
        // Apply for a regular expression
        try {
          Pattern pattern = Pattern.compile(originalText);
          Matcher matcher = pattern.matcher(text);
          while (matcher.find()) {
            String foundText = matcher.group();
            int start = matcher.start();
            int end = matcher.end();
            int currentLength = tmpText.length();
            matcher.appendReplacement(tmpText, replacementText);
            if (replacements != null) {
              String replacedBy = tmpText.substring(currentLength + start - currentIndex);
              String comment = foundText + " → " + replacedBy;
              if (!replacements.contains(comment)) {
                replacements.add(comment);
              }
            }
            currentIndex = end;
          }
        } catch (PatternSyntaxException e) {
          System.err.println("Error with " + originalText + ": " + e.getMessage());
        }
      } else {
        // Apply for a basic expression
        boolean finished = false;
        while (!finished) {
          int newIndex = text.indexOf(originalText, currentIndex);
          if (newIndex < 0) {
            finished = true;
          } else {
            if (newIndex > currentIndex) {
              tmpText.append(text.substring(currentIndex, newIndex));
              currentIndex = newIndex;
            }
            tmpText.append(replacementText);
            currentIndex += originalText.length();
            if (replacements != null) {
              String comment = originalText + " → " + replacementText;
              if (!replacements.contains(comment)) {
                replacements.add(comment);
              }
            }
          }
        }
      }
      if (currentIndex > 0) {
        if (currentIndex < text.length()) {
          tmpText.append(text.substring(currentIndex));
        }
        text = tmpText.toString();
      }
    }

    return text;
  }

  /**
   * @param af Other automatic fixing expression.
   * @return Comparison of the two automatic fixing expressions.
   * @see java.lang.Comparable#compareTo(java.lang.Object)
   */
  public int compareTo(AutomaticFixing af) {
    if (af == null) {
      return -1;
    }
    int compare = originalText.compareTo(af.getOriginalText());
    if (compare != 0) {
      return compare;
    }
    return replacementText.compareTo(af.getReplacementText());
  }

  /**
   * @return String representation of the automatic fixing expression.
   * @see java.lang.Object#toString()
   */
  @Override
  public String toString() {
    return originalText + " → " + replacementText;
  }
}

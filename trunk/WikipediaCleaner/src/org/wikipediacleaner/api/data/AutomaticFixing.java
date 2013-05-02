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

  public AutomaticFixing() {
    //
  }

  public AutomaticFixing(String from, String to, boolean regex) {
    originalText = from;
    replacementText = to;
    this.regex = regex;
  }

  public String getOriginalText() {
    return originalText;
  }

  public void setOriginalText(String text) {
    originalText = text;
  }

  public String getReplacementText() {
    return replacementText;
  }

  public void setReplacementText(String text) {
    replacementText = text;
  }

  public boolean isRegex() {
    return regex;
  }

  public void setRegex(boolean flag) {
    regex = flag;
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
    return "[" + originalText + "] → [" + replacementText + "]";
  }
}

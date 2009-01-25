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
public class AutomaticFixing {
  private String originalText;
  private String replacementText;

  public AutomaticFixing() {
    //
  }

  public AutomaticFixing(String from, String to) {
    originalText = from;
    replacementText = to;
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

  /* (non-Javadoc)
   * @see java.lang.Object#toString()
   */
  @Override
  public String toString() {
    return "[" + originalText + "] -> [" + replacementText + "]";
  }
}

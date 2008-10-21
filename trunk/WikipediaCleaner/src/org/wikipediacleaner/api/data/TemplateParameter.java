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
 * Utility class for memorizing template parameters.
 */
public class TemplateParameter {

  private String name;
  private String value;
  private boolean relevant;

  /**
   * @param name Parameter name.
   * @param value Parameter value.
   */
  public TemplateParameter(String name, String value) {
    this.name = name;
    this.value = value;
    this.relevant = false;
  }

  /**
   * @return Parameter name.
   */
  public String getName() {
    return name;
  }

  /**
   * @return Parameter value.
   */
  public String getValue() {
    return value;
  }

  /**
   * @return Flag indicating if the parameter is relevant.
   */
  public boolean isRelevant() {
    return relevant;
  }

  /**
   * @param relevant Flag indicating if the parameter is relevant.
   */
  public void setRelevant(boolean relevant) {
    this.relevant = relevant;
  }
}

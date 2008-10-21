/*
 *  WikipediaCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2007  Nicolas Vervelle
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
 * A simple class to list templates to look for when searching for disambiguation links.
 */
public class TemplateMatch {

  private final String name;
  private final String[] parameters;
  private final String defaultParameters;
  private final boolean good;
  private final boolean helpNeeded;

  public TemplateMatch(
      String template,
      String parameter,
      String defaultParameters,
      boolean good,
      boolean helpNeeded) {
    this.name = template;
    this.parameters = parameter.split(",");
    this.defaultParameters = defaultParameters;
    this.good = good;
    this.helpNeeded = helpNeeded;
  }

  public String getName() {
    return name;
  }

  public int getParametersCount() {
    return (parameters != null) ? parameters.length : 0;
  }

  public String getParameter(int index) {
    return (parameters != null) ? parameters[index] : null;
  }

  public String getDefaultParameters() {
    return defaultParameters;
  }

  public boolean isGood() {
    return good;
  }

  public boolean isHelpNeeded() {
    return helpNeeded;
  }
}

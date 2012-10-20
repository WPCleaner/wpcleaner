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

package org.wikipediacleaner.api.check.algorithm;

import java.util.Collection;
import java.util.Map;

import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.constants.CWConfigurationError;
import org.wikipediacleaner.api.data.PageAnalysis;
import org.wikipediacleaner.gui.swing.component.MWPane;


/**
 * Interface implemented by all errors detected by the check wikipedia project.
 */
public interface CheckErrorAlgorithm {

  /**
   * @return Flag indicating if this algorithm is available.
   */
  public boolean isAvailable();

  /**
   * @param configuration Configuration of the error.
   */
  public void setConfiguration(CWConfigurationError configuration);

  /**
   * @return Short description of the error.
   * (See Check Wikipedia project for the description of errors)
   */
  public String getShortDescription();

  /**
   * @return Short description of the error.
   * (See Check Wikipedia project for the description of errors)
   */
  public String getShortDescriptionReplaced();

  /**
   * @return Long description of the error.
   * (See Check Wikipedia project for the description of errors)
   */
  public String getLongDescription();

  /**
   * @return Flag indicating if the detection is fully done.
   */
  public boolean isFullDetection();

  /**
   * @return Link to error description.
   */
  public String getLink();

  /**
   * Tell if a page is among the white list.
   * 
   * @param title Page title.
   * @return Page among the white list ?
   */
  public boolean isInWhiteList(String title);

  /**
   * @return White list page name.
   */
  public String getWhiteListPageName();

  /**
   * @return Priority.
   */
  public int getPriority();

  /**
   * @return Error number.
   * (See Check Wikipedia project for the description of errors)
   */
  public String getErrorNumberString();

  /**
   * @return Error number.
   * (See Check Wikipedia project for the description of errors)
   */
  public int getErrorNumber();


  /**
   * @param name Property name.
   * @param useWiki Flag indicating if wiki configuration can be used.
   * @param useGeneral Flag indicating if general configuration can be used.
   * @param acceptEmpty Flag indicating if empty strings are accepted.
   * @return Property value.
   */
  public String getSpecificProperty(
      String name,
      boolean useWiki, boolean useGeneral, boolean acceptEmpty);

  /**
   * Analyze a page to check if errors are present.
   * 
   * @param pageAnalysis Page analysis.
   * @param errors Errors found in the page.
   * @return Flag indicating if the error was found.
   */
  public boolean analyze(PageAnalysis pageAnalysis, Collection<CheckErrorResult> errors);

  /**
   * Return the parameters used to configure the algorithm.
   * 
   * @return Map of parameters (Name -> description).
   */
  public Map<String, String> getParameters();

  /**
   * Automatic fixing of all the errors in the page.
   * 
   * @param analysis Page analysis.
   * @return Page contents after fix.
   */
  public String automaticFix(PageAnalysis analysis);

  /**
   * @return List of possible global fixes.
   */
  public String[] getGlobalFixes();

  /**
   * Fix all the errors in the page.
   * 
   * @param fixName Fix name (extracted from getGlobalFixes()).
   * @param analysis Page analysis.
   * @param textPane Text pane.
   * @return Page contents after fix.
   */
  public String fix(String fixName, PageAnalysis analysis, MWPane textPane);
}

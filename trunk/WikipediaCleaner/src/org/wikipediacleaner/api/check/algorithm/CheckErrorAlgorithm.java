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

import java.util.List;
import java.util.Map;

import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.data.Page;


/**
 * Interface implemented by all errors detected by the check wikipedia project.
 */
public interface CheckErrorAlgorithm {

  /**
   * @return Flag indicating if this algorithm is available.
   */
  public boolean isAvailable();

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
   * @param desc Short description.
   */
  public void setShortDescription(String desc);

  /**
   * @return Long description of the error.
   * (See Check Wikipedia project for the description of errors)
   */
  public String getLongDescription();

  /**
   * @param desc Long description.
   */
  public void setLongDescription(String desc);

  /**
   * @return Flag indicating if the detection is fully done.
   */
  public boolean isFullDetection();

  /**
   * @return Link to error description.
   */
  public String getLink();

  /**
   * @param link Link to error description.
   */
  public void setLink(String link);

  /**
   * Tell if a page is among the white list.
   * 
   * @param title Page title.
   * @return Page among the white list ?
   */
  public boolean isInWhiteList(String title);

  /**
   * @param whiteList White list.
   */
  public void setWhiteList(String[] whiteList);

  /**
   * @return Priority.
   */
  public int getPriority();

  /**
   * @param priority Priority.
   */
  public void setPriority(int priority);

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
   * Analyze a page to check if errors are present.
   * 
   * @param page Page.
   * @param contents Page contents (may be different from page.getContents()).
   * @param errors Errors found in the page.
   * @return Flag indicating if the error was found.
   */
  public boolean analyze(Page page, String contents, List<CheckErrorResult> errors);

  /**
   * Return the parameters used to configure the algorithm.
   * 
   * @return Map of parameters (Name -> description).
   */
  public Map<String, String> getParameters();

  /**
   * Automatic fixing of all the errors in the page.
   * 
   * @param page Page.
   * @param contents Page contents (may be different from page.getContents()).
   * @return Page contents after fix.
   */
  public String automaticFix(Page page, String contents);

  /**
   * @return List of possible global fixes.
   */
  public String[] getGlobalFixes();

  /**
   * Fix all the errors in the page.
   * 
   * @param fixName Fix name (extracted from getGlobalFixes()).
   * @param page Page.
   * @param contents Page contents (may be different from page.getContents()).
   * @return Page contents after fix.
   */
  public String fix(String fixName, Page page, String contents);
}

/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm;

import java.util.Collection;
import java.util.List;
import java.util.Map;

import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.constants.CWConfigurationError;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.data.PageAnalysis;
import org.wikipediacleaner.gui.swing.component.MWPane;


/**
 * Interface implemented by all errors detected by the check wikipedia project.
 */
public interface CheckErrorAlgorithm {

  public final static int MAX_ERROR_NUMBER_WITH_LIST = 500;

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
   * @return True if the error has a list of pages.
   */
  public boolean hasList();

  /**
   * @return True if the error has a special list of pages.
   */
  public boolean hasSpecialList();

  /**
   * Retrieve the list of pages in error.
   * 
   * @param wiki Wiki.
   * @param limit Maximum number of pages to retrieve.
   * @return List of pages in error.
   */
  public List<Page> getSpecialList(EnumWikipedia wiki, int limit);

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
   * @param analysis Page analysis.
   * @param errors Errors found in the page.
   * @param onlyAutomatic True if analysis could be restricted to errors automatically fixed.
   * @return Flag indicating if the error was found.
   */
  public boolean analyze(PageAnalysis analysis, Collection<CheckErrorResult> errors, boolean onlyAutomatic);

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
   * Bot fixing of all the errors in the page.
   * 
   * @param analysis Page analysis.
   * @return Page contents after fix.
   */
  public String botFix(PageAnalysis analysis);

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

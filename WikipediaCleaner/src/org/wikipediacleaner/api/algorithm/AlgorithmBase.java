/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.algorithm;

import java.util.HashMap;
import java.util.Map;

import org.wikipediacleaner.api.constants.CWConfiguration;
import org.wikipediacleaner.api.constants.WPCConfiguration;
import org.wikipediacleaner.api.constants.WikiConfiguration;

/**
 * Abstract base class for analyzing errors.
 */
public abstract class AlgorithmBase implements Algorithm {

  /** Internal name of the error */
  private final String name;

  /**
   * @param name Name of the error.
   */
  public AlgorithmBase(String name) {
    this.name = name;
  }

  /**
   * @return Name of the error.
   */
  public String getName() {
    return name;
  }

  /* ========================================================================== */
  /* General information about the algorithm                                    */
  /* ========================================================================== */

  /**
   * @return Short description of the error.
   * (See Check Wikipedia project for the description of errors)
   */
  @Override
  public String getShortDescription() {
    return getName();
  }

  /**
   * @return Short description of the error.
   */
  @Override
  public String getShortDescriptionReplaced() {
    return getShortDescription();
  }

  /**
   * @return Long description of the error.
   * (See Check Wikipedia project for the description of errors)
   */
  @Override
  public String getLongDescription() {
    return getName();
  }

  /**
   * @return Link to error description.
   */
  @Override
  public String getLink() {
    return null;
  }

  /**
   * @return Flag indicating if this algorithm is available.
   */
  @Override
  public boolean isAvailable() {
    return true;
  }

  /* ========================================================================== */
  /* Configuration management                                                   */
  /* ========================================================================== */

  /** Configuration for the wiki. */
  private WikiConfiguration wikiConfiguration;

  /** Configuration for WPCleaner. */
  private WPCConfiguration wpcConfiguration;

  /** Configuration for Check Wiki. */
  private CWConfiguration cwConfiguration;

  /** List of parameters for the algorithm */
  private Map<String, AlgorithmParameter> params = new HashMap<>();

  /**
   * @param wikiConfiguration Configuration for the wiki.
   * @param cwConfiguration Configuration for Check Wiki.
   * @param wpcConfiguration Configuration for WPCleaner.
   * @see org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithm#setConfiguration(
   *         org.wikipediacleaner.api.constants.WikiConfiguration,
   *         org.wikipediacleaner.api.constants.CWConfiguration,
   *         org.wikipediacleaner.api.constants.WPCConfiguration)
   */
  @Override
  public void setConfiguration(
      WikiConfiguration wikiConfiguration,
      CWConfiguration cwConfiguration,
      WPCConfiguration wpcConfiguration) {
    this.wikiConfiguration = wikiConfiguration;
    this.wpcConfiguration = wpcConfiguration;
    this.cwConfiguration = cwConfiguration;
    params.clear();
    addParameters();
    initializeSettings();
  }

  /**
   * @return Configuration for the wiki.
   */
  protected WikiConfiguration getWikiConfiguration() {
    return wikiConfiguration;
  }

  /**
   * @return Configuration for WPCleaner.
   */
  protected WPCConfiguration getWPCConfiguration() {
    return wpcConfiguration;
  }

  /**
   * @return Configuration for Check Wiki.
   */
  protected CWConfiguration getCWConfiguration() {
    return cwConfiguration;
  }

  /**
   * Initialize settings for the algorithm.
   */
  protected void initializeSettings() {
    // Nothing to do for base class
  }

  /**
   * Return the parameters used to configure the algorithm.
   * 
   * @return Map of parameters (key=name, value=description).
   */
  @Override
  public Map<String, AlgorithmParameter> getParameters() {
    return params;
  }

  /**
   * Build the list of parameters for this algorithm.
   */
  protected void addParameters() {
    // No parameters by default
  }

  /**
   * Add a parameter to the list of parameters for this algorithm.
   * 
   * @param parameter Parameter to be added.
   */
  protected final void addParameter(AlgorithmParameter parameter) {
    if (parameter == null) {
      return;
    }
    params.put(parameter.getName(), parameter);
  }
}

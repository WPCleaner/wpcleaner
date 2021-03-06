/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2014  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.api.dataaccess;

import org.wikipediacleaner.api.data.Page;


/**
 * Interface for providing access to a page.
 */
public interface PageProvider extends WikiProvider {

  /**
   * @return Page.
   */
  public Page getPage();
}

/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2014  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.api.check;

import org.wikipediacleaner.api.data.Page;


/**
 * Interface for listening to Check Wiki events.
 */
public interface CheckWikiListener {

  /**
   * Callback called when a page is fixed.
   * 
   * @param page Page fixed.
   * @param errorNumber Error for which the page is fixed.
   */
  public void pageFixed(Page page, int errorNumber);
}

/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api;

import java.util.Date;
import java.util.List;

import org.wikipediacleaner.api.data.RecentChange;


/**
 * Listener interface for Recent changes.
 */
public interface RecentChangesListener {

  /**
   * Invoked when Recent changes occured.
   * 
   * @param rc Recent changes.
   * @param currentTime Current time.
   */
  public void recentChanges(List<RecentChange> rc, Date currentTime);
}

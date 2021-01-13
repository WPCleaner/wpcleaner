/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.impl;

import java.text.ParseException;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import org.wikipediacleaner.api.API;
import org.wikipediacleaner.api.APIException;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.DataManager;
import org.wikipediacleaner.api.data.RecentChange;


/**
 * Runnable for querying recent changes.
 */
class RecentChangesRunnable implements Runnable {

  /**
   * Recent changes manager.
   */
  private final RecentChangesManager manager;

  /**
   * Wiki.
   */
  private final EnumWikipedia wiki;

  /**
   * API.
   */
  private final API api;

  /**
   * Start for next recent changes request.
   */
  private String start;

  /**
   * Flag set when it is requested to stop querying for recent changes.
   */
  private boolean shouldStop;

  /**
   * @param manager Recent changes manager.
   * @param wiki Wiki.
   * @param api API.
   */
  public RecentChangesRunnable(
      RecentChangesManager manager,
      EnumWikipedia wiki, API api) {
    this.manager = manager;
    this.wiki = wiki;
    this.api = api;
  }

  /**
   * Regularly query the API for recent changes.
   * 
   * @see java.lang.Runnable#run()
   */
  @Override
  public void run() {
    while (!shouldStop) {
      try {
        List<RecentChange> recentChanges = new ArrayList<>();
        start = api.getRecentChanges(wiki, start, recentChanges);
        if (!recentChanges.isEmpty()) {
          Date currentTime = DataManager.convertIso8601DateTime(start);
          manager.fireRecentChanges(recentChanges, currentTime);
        }
      } catch (APIException e) {
        // Nothing to do.
      } catch (ParseException e) {
        // Nothing to do.
      }
      try {
        Thread.sleep(30000);
      } catch (InterruptedException e) {
        // Nothing to do.
      }
    }
  }

  /**
   * Called to stop querying for recent changes.
   */
  public void shouldStop() {
    shouldStop = true;
  }
}

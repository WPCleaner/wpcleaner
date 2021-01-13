/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.impl;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import org.wikipediacleaner.api.API;
import org.wikipediacleaner.api.RecentChangesListener;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.RecentChange;


/**
 * Utility class to manage Recent changes.
 */
class RecentChangesManager {

  /**
   * Wiki.
   */
  private final EnumWikipedia wiki;

  /**
   * API.
   */
  private final API api;

  /**
   * Listeners.
   */
  private final List<RecentChangesListener> listeners = new ArrayList<>();

  /**
   * Runnabel for querying recent changes.
   */
  private RecentChangesRunnable runnable;

  /**
   * @param wiki Wiki.
   * @param api API.
   */
  public RecentChangesManager(EnumWikipedia wiki, API api) {
    this.wiki = wiki;
    this.api = api;
  }

  /**
   * Adds a <code>RecentChangesListener</code> to the API.
   * 
   * @param listener Recent changes listener.
   */
  public void addRecentChangesListener(
      RecentChangesListener listener) {
    listeners.add(listener);
    listenersChanged();
  }

  /**
   * Removes a <code>RecentChangesListener</code> from the API.
   * 
   * @param listener Recent changes listener.
   */
  public void removeRecentChangesListener(
      RecentChangesListener listener) {
    listeners.remove(listener);
    listenersChanged();
  }

  /**
   * Invoked when Recent changes occured.
   * 
   * @param rc Recent changes.
   * @param currentTime Current time.
   */
  public void fireRecentChanges(List<RecentChange> rc, Date currentTime) {
    for (RecentChangesListener listener : listeners) {
      listener.recentChanges(rc, currentTime);
    }
  }

  /**
   * Event fired when the list of listeners has changed.
   */
  private void listenersChanged() {
    if (listeners.isEmpty()) {
      if (runnable != null) {
        runnable.shouldStop();
      }
    } else {
      if (runnable == null) {
        runnable = new RecentChangesRunnable(this, wiki, api);
        new Thread(runnable, "RecentChanges-" + wiki.getSettings().getCode()).start();
      }
    }
  }
}

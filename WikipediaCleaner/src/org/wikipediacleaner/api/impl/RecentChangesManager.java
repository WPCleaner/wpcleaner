/*
 *  WikipediaCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
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
  private final List<RecentChangesListener> listeners = new ArrayList<RecentChangesListener>();

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

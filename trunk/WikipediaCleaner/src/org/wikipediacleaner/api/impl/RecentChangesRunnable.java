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
  public void run() {
    while (!shouldStop) {
      try {
        List<RecentChange> recentChanges = new ArrayList<RecentChange>();
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

/*
 *  WikipediaCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2007  Nicolas Vervelle
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

package org.wikipediacleaner.api.execution;

import java.util.List;

import org.wikipediacleaner.api.MediaWikiListener;
import org.wikipediacleaner.api.base.API;
import org.wikipediacleaner.api.base.APIException;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.i18n.GT;


/**
 * A Callable implementation for retrieving Disambiguation Status.
 */
public class DisambiguationStatusCallable extends MediaWikiCallable<List<Page>> {

  private final List<Page> pages;

  /**
   * @param wikipedia Wikipedia.
   * @param listener Listener of MediaWiki events.
   * @param api MediaWiki API.
   * @param pages List of pages.
   */
  public DisambiguationStatusCallable(
      EnumWikipedia wikipedia, MediaWikiListener listener, API api,
      List<Page> pages) {
    super(wikipedia, listener, api);
    this.pages = pages;
  }

  /* (non-Javadoc)
   * @see java.util.concurrent.Callable#call()
   */
  public List<Page> call() throws APIException {
    setText(GT._("Retrieving disambiguation informations"));
    api.initializeDisambiguationStatus(getWikipedia(), pages);
    return pages;
  }

}

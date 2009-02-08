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

package org.wikipediacleaner.gui.swing.worker;

import java.util.HashMap;
import java.util.Properties;

import org.wikipediacleaner.api.MediaWiki;
import org.wikipediacleaner.api.base.APIException;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.gui.swing.basic.BasicWindow;
import org.wikipediacleaner.gui.swing.basic.BasicWorker;

/**
 * SwingWorker for automatic disambiguation. 
 */
public class AutomaticDisambiguationWorker extends BasicWorker {

  private final Page[] pages;
  private final HashMap<String, Properties> replacements;
  private final EnumWikipedia wikipedia;
  private final String comment;
  private final StringBuffer description;
  private final boolean showDescription;

  public AutomaticDisambiguationWorker(
      BasicWindow window, Page[] pages,
      HashMap<String, Properties> replacements,
      EnumWikipedia wikipedia, String comment,
      boolean showDescription) {
    super(window);
    this.pages = pages.clone();
    this.replacements = replacements;
    this.wikipedia = wikipedia;
    this.comment = comment;
    this.showDescription = showDescription;
    this.description = (showDescription ? new StringBuffer() : null);
  }

  /* (non-Javadoc)
   * @see org.wikipediacleaner.gui.swing.utils.SwingWorker#construct()
   */
  @Override
  public Object construct() {
    try {
      MediaWiki mw = MediaWiki.getMediaWikiAccess(this);
      mw.replaceText(pages, replacements, wikipedia, comment, description);
      if ((showDescription) && (description.length() > 0)) {
        System.out.println(description);
      }
    } catch (APIException e) {
      return e;
    }
    return null;
  }
}
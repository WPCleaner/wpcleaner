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

import java.util.List;
import java.util.Map;

import org.wikipediacleaner.api.APIException;
import org.wikipediacleaner.api.MediaWiki;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.AutomaticFixing;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.gui.swing.InformationWindow;
import org.wikipediacleaner.gui.swing.basic.BasicWindow;
import org.wikipediacleaner.gui.swing.basic.BasicWorker;
import org.wikipediacleaner.i18n.GT;

/**
 * SwingWorker for automatic disambiguation. 
 */
public class AutomaticDisambiguationWorker extends BasicWorker {

  private final Page[] pages;
  private final Map<String, List<AutomaticFixing>> replacements;
  private final String comment;
  private final StringBuilder description;
  private final boolean showDescription;

  public AutomaticDisambiguationWorker(
      EnumWikipedia wikipedia, BasicWindow window,
      Page[] pages, Map<String, List<AutomaticFixing>> replacements,
      String comment, boolean showDescription) {
    super(wikipedia, window);
    this.pages = pages.clone();
    this.replacements = replacements;
    this.comment = comment;
    this.showDescription = showDescription;
    this.description = (showDescription ? new StringBuilder() : null);
  }

  /* (non-Javadoc)
   * @see org.wikipediacleaner.gui.swing.utils.SwingWorker#construct()
   */
  @Override
  public Object construct() {
    try {
      MediaWiki mw = MediaWiki.getMediaWikiAccess(this);
      Integer count = Integer.valueOf(mw.replaceText(
          pages, replacements, getWikipedia(), comment, description));
      if (showDescription && (count > 0)) {
        InformationWindow.createInformationWindow(
            GT._("The following modifications have been done ({0} pages):", count.toString()),
            description.toString(), getWikipedia());
      }
      return count;
    } catch (APIException e) {
      return e;
    }
  }
}
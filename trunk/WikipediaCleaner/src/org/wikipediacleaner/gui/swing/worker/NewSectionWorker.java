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

import org.wikipediacleaner.api.base.API;
import org.wikipediacleaner.api.base.APIException;
import org.wikipediacleaner.api.base.APIFactory;
import org.wikipediacleaner.gui.swing.basic.BasicWindow;
import org.wikipediacleaner.gui.swing.basic.BasicWorker;
import org.wikipediacleaner.i18n.GT;

/**
 * SwingWorker for sending the new section content. 
 */
public class NewSectionWorker extends BasicWorker {

  private final String page;
  private final String section;
  private final String text;
  private final String editToken;
  private final boolean forceWatch;

  public NewSectionWorker(BasicWindow window, String page, String section, String editToken, String text, boolean forceWatch) {
    super(window);
    this.page = page;
    this.section = section;
    this.text = text;
    this.editToken = editToken;
    this.forceWatch = forceWatch;
  }

  /* (non-Javadoc)
   * @see org.wikipediacleaner.gui.swing.utils.SwingWorker#finished()
   */
  @Override
  public void finished() {
    super.finished();
    getWindow().dispose();
  }

  /* (non-Javadoc)
   * @see org.wikipediacleaner.gui.swing.utils.SwingWorker#construct()
   */
  @Override
  public Object construct() {
    try {
      setText(GT._("Retrieving MediaWiki API"));
      API api = APIFactory.getAPI();
      setText(GT._("Adding comment"));
      api.addNewSection(page, section, text, editToken, forceWatch);
    } catch (APIException e) {
      return e;
    }
    return null;
  }
}
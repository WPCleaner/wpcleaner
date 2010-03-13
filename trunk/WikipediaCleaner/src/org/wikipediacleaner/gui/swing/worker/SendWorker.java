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
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.gui.swing.basic.BasicWindow;
import org.wikipediacleaner.gui.swing.basic.BasicWorker;
import org.wikipediacleaner.i18n.GT;

/**
 * SwingWorker for sending the new page content. 
 */
public class SendWorker extends BasicWorker {

  private final Page page;
  private final String text;
  private final String comment;
  private final EnumWikipedia wikipedia;

  public SendWorker(
      BasicWindow window,
      Page page, String text, String comment,
      EnumWikipedia wikipedia) {
    super(window);
    this.page = page;
    this.text = text;
    this.comment = comment;
    this.wikipedia = wikipedia;
  }

  /* (non-Javadoc)
   * @see org.wikipediacleaner.gui.swing.utils.SwingWorker#construct()
   */
  @Override
  public Object construct() {
    setText(GT._("Retrieving MediaWiki API"));
    API api = APIFactory.getAPI();
    try {
      setText(GT._("Updating page contents"));
      api.updatePage(page, text, wikipedia.createUpdatePageComment(comment, null));
    } catch (APIException e) {
      if (APIException.ERROR_BAD_TOKEN.equals(e.getErrorCode())) {
        try {
          setText(GT._("Error 'badtoken' detected: Retrying"));
          api.retrieveContents(page, false);
          api.updatePage(page, text, wikipedia.createUpdatePageComment(comment, null));
        } catch (APIException e2) {
          return e2;
        }
      } else {
        return e;
      }
    }
    return null;
  }
}
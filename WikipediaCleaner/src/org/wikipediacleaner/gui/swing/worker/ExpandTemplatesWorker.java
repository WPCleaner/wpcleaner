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

import javax.swing.text.JTextComponent;

import org.wikipediacleaner.api.MediaWiki;
import org.wikipediacleaner.api.base.APIException;
import org.wikipediacleaner.gui.swing.basic.BasicWindow;
import org.wikipediacleaner.gui.swing.basic.BasicWorker;

/**
 * SwingWorker for expanding templates. 
 */
public class ExpandTemplatesWorker extends BasicWorker {

  private String title;
  private JTextComponent textOriginal;
  private JTextComponent textExpanded;
  private HtmlPreview htmlPreview;

  public ExpandTemplatesWorker(
      BasicWindow window, String title,
      JTextComponent textOriginal,
      JTextComponent textExpanded,
      HtmlPreview htmlPreview) {
    super(window);
    this.title = title;
    this.textOriginal = textOriginal;
    this.textExpanded = textExpanded;
    this.htmlPreview = htmlPreview;
  }

  /* (non-Javadoc)
   * @see org.wikipediacleaner.gui.swing.utils.SwingWorker#construct()
   */
  @Override
  public Object construct() {
    try {
      MediaWiki mw = MediaWiki.getMediaWikiAccess(this);
      if (textExpanded != null) {
        textExpanded.setText(mw.expandTemplates(title, textOriginal.getText()));
      }
      if (htmlPreview != null) {
        String text = mw.parseText(title, textOriginal.getText());
        htmlPreview.setHtml(
            "<html><head>" +
            "</head><body>" + text + "</body></html>");
      }
    } catch (APIException e) {
      return e;
    }
    return null;
  }
}
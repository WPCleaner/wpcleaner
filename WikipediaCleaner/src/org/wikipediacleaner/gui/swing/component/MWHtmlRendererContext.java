/*
 *  WikipediaCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2008  Nicolas Vervelle
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

package org.wikipediacleaner.gui.swing.component;

import java.net.URISyntaxException;
import java.net.URL;

import org.lobobrowser.html.HtmlRendererContext;
import org.lobobrowser.html.UserAgentContext;
import org.lobobrowser.html.gui.HtmlPanel;
import org.lobobrowser.html.test.SimpleHtmlRendererContext;
import org.w3c.dom.html2.HTMLElement;
import org.wikipediacleaner.gui.swing.basic.Utilities;


/**
 * A simple HTML renderer context for MediaWiki.
 */
public class MWHtmlRendererContext extends SimpleHtmlRendererContext {

  /**
   * @param contextComponent
   * @param ucontext
   */
  public MWHtmlRendererContext(HtmlPanel contextComponent,
      UserAgentContext ucontext) {
    super(contextComponent, ucontext);

  }

  /**
   * @param contextComponent
   * @param parentRcontext
   */
  public MWHtmlRendererContext(HtmlPanel contextComponent,
      HtmlRendererContext parentRcontext) {
    super(contextComponent, parentRcontext);

  }

  /**
   * Callback called when a link is clicked.
   * 
   * @param linkNode Node selected.
   * @param url URL for destination.
   * @param target Target.
   */
  @Override
  public void linkClicked(HTMLElement linkNode, URL url, String target) {
    if (Utilities.isDesktopSupported()) {
      try {
        Utilities.browseURL(url.toURI());
      } catch (URISyntaxException e) {
        //
      }
    }
  }

}

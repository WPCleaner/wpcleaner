/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
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

/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.gui.swing.worker;

import javax.swing.text.JTextComponent;

import org.wikipediacleaner.api.APIException;
import org.wikipediacleaner.api.MediaWiki;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.gui.swing.basic.BasicWindow;
import org.wikipediacleaner.gui.swing.basic.BasicWorker;

/**
 * SwingWorker for expanding templates. 
 */
public class ExpandTemplatesWorker extends BasicWorker {

  private final String title;
  private final JTextComponent textOriginal;
  private final JTextComponent textExpanded;
  private final HtmlPreview htmlPreview;

  /**
   * @param wikipedia Wikipedia.
   * @param window Window.
   * @param title
   * @param textOriginal
   * @param textExpanded
   * @param htmlPreview
   */
  public ExpandTemplatesWorker(
      EnumWikipedia wikipedia, BasicWindow window, String title,
      JTextComponent textOriginal,
      JTextComponent textExpanded,
      HtmlPreview htmlPreview) {
    super(wikipedia, window);
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
        textExpanded.setText(mw.expandTemplates(getWikipedia(), title, textOriginal.getText()));
      }
      if (htmlPreview != null) {
        String text = mw.parseText(getWikipedia(), title, textOriginal.getText());
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
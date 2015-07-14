/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2015  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.gui.swing.component;

import java.beans.EventHandler;
import java.net.URISyntaxException;

import javax.swing.JEditorPane;
import javax.swing.event.HyperlinkEvent;
import javax.swing.event.HyperlinkListener;

import org.wikipediacleaner.gui.swing.basic.Utilities;


/**
 * An editor pane for displaying HTML pages.
 */
public class HTMLPane extends JEditorPane {

  /** Serialization */
  private static final long serialVersionUID = 6677696793879629708L;

  /**
   * @param text The text to initialize with; may be <code>null</code>.
   */
  private HTMLPane(String text) {
    super("text/html", text);
  }

  /**
   * @param html HTML text.
   * @return HTML pane.
   */
  public static HTMLPane createHTMLPane(String html) {
    HTMLPane pane = new HTMLPane(html);
    pane.setEditable(false);
    pane.addHyperlinkListener(EventHandler.create(
        HyperlinkListener.class, pane, "hyperLink", ""));
    return pane;
  }

  /**
   * @param event Event triggering the call.
   */
  public void hyperLink(HyperlinkEvent event) {
    if (event == null) {
      return;
    }
    if (event.getEventType() == HyperlinkEvent.EventType.ACTIVATED) {
      try {
        Utilities.browseURL(event.getURL().toURI());
      } catch (URISyntaxException e) {
        // Nothing to do
      }
    }
  }
}

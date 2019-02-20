/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2019  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.gui.swing.component.simple;

import java.awt.Component;
import java.awt.event.ActionListener;
import java.beans.EventHandler;

import javax.swing.JButton;
import javax.swing.JComponent;

import org.wikipediacleaner.gui.swing.basic.Utilities;
import org.wikipediacleaner.i18n.GT;
import org.wikipediacleaner.utils.ConfigurationValueShortcut;

/**
 * An helper class to build a help button.
 */
public class IdeaButton {

  /** Talk page for submitting ideas or bug reports */
  private final static String URL_TALK_PAGE       = "https://en.wikipedia.org/wiki/Wikipedia_talk:WPCleaner";

  /** Parent component */
  private final Component parentComponent;

  /** Button for ideas */
  private final JButton button;

  /**
   * Constructor.
   * 
   * @param parentComponent Parent component.
   */
  public IdeaButton(Component parentComponent) {
    this.parentComponent = parentComponent;
    
    // Create button
    button = Utilities.createJButton(
        GT._T("Idea? Bug?"),
        ConfigurationValueShortcut.BUG_REPORT);
    button.addActionListener(EventHandler.create(
        ActionListener.class, this, "actionIdea"));
  }

  /**
   * @return Button for ideas.
   */
  public JComponent getButton() {
    return button;
  }

  /**
   * Action called when Idea button is pressed.
   */
  public void actionIdea() {
    String url = URL_TALK_PAGE;
    if (Utilities.isDesktopSupported()) {
      Utilities.browseURL(url);
    } else {
      Utilities.displayUrlMessage(
          parentComponent,
          GT._T("You can submit bug reports or feature requests at the following URL:"),
          url);
    }
  }
}

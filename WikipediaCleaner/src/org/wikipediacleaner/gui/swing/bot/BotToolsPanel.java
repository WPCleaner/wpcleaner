/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.gui.swing.bot;

import java.awt.LayoutManager;

import javax.swing.JPanel;

import org.wikipediacleaner.gui.swing.basic.BasicWindow;


/**
 * Base class for a Bot panel.
 */
abstract class BotToolsPanel extends JPanel {

  /**
   * Serialization.
   */
  private static final long serialVersionUID = -6986151664537866639L;

  protected final BasicWindow window;

  /**
   * Construct a Bot panel.
   * 
   * @param layout Layout manager.
   */
  public BotToolsPanel(BasicWindow window, LayoutManager layout) {
    super(layout);
    this.window = window;
  }

  /**
   * Update components state.
   */
  protected void updateComponentState() {
    // Nothing done by default
  }
}

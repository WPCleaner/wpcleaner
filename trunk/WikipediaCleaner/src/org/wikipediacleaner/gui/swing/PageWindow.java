/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.gui.swing;

import java.awt.event.ActionListener;
import java.awt.event.ItemListener;
import java.beans.PropertyChangeListener;

import org.wikipediacleaner.api.constants.Contributions;
import org.wikipediacleaner.gui.swing.basic.BasicWindow;


/**
 * A base class for Wikipedia Cleaner windows with page contents.
 */
public abstract class PageWindow
  extends BasicWindow
  implements ActionListener, ItemListener, PropertyChangeListener {

  /**
   * @return Contributions.
   */
  public Contributions getContributions() {
    return null;
  }
}

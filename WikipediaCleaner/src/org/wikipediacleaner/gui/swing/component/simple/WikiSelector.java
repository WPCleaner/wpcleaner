/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2019  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.gui.swing.component.simple;

import java.awt.Component;
import java.awt.event.ActionListener;
import java.awt.event.ItemListener;
import java.beans.EventHandler;
import java.util.ArrayList;
import java.util.List;

import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JToolBar;
import javax.swing.SwingConstants;

import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.dataaccess.WikiProvider;
import org.wikipediacleaner.gui.swing.basic.Utilities;
import org.wikipediacleaner.i18n.GT;
import org.wikipediacleaner.images.EnumImageSize;
import org.wikipediacleaner.utils.Configuration;

/**
 * An helper class to build a combo box for selecting a wiki.
 */
public class WikiSelector implements WikiProvider {

  /** URL for other wiki than the defined ones */
  private final static String URL_OTHER_WIKI = "http://en.wikipedia.org/wiki/Wikipedia:WPCleaner/Wikis";

  /** Parent component */
  private final Component parentComponent;

  /** Combo box for selecting a wiki */
  private final JComboBox<EnumWikipedia> combo;

  /** Label to be associated with the combo box */
  private final JLabel label;

  /** Button to be associated with the combo box */
  private final JToolBar toolbar;

  /** Listener for changes in the wiki selection */
  private final List<WikiChangeListener> changeListeners;

  /**
   * Constructor.
   * 
   * @param parentComponent Parent component.
   */
  public WikiSelector(Component parentComponent) {
    this.parentComponent = parentComponent;
    this.changeListeners = new ArrayList<>();
    
    // Create combo box
    Configuration configuration = Configuration.getConfiguration();
    EnumWikipedia defaultWikipedia = configuration.getWikipedia();
    combo = new JComboBox<>(EnumWikipedia.getList().toArray(new EnumWikipedia[0]));
    combo.setEditable(false);
    combo.setSelectedItem(defaultWikipedia);
    combo.addItemListener(EventHandler.create(
        ItemListener.class, this, "notifyWikiChange"));

    // Create label
    label = Utilities.createJLabel(GT._T("Wiki"));
    label.setLabelFor(combo);
    label.setHorizontalAlignment(SwingConstants.TRAILING);

    // Create button
    toolbar = new JToolBar(SwingConstants.HORIZONTAL);
    toolbar.setFloatable(false);
    toolbar.setBorderPainted(false);
    JButton buttonWikiInfo = Utilities.createJButton(
        "tango-help-browser.png", EnumImageSize.SMALL,
        GT._T("Other Wiki"), false, null);
    buttonWikiInfo.addActionListener(EventHandler.create(
        ActionListener.class, this, "actionOtherWiki"));
    toolbar.add(buttonWikiInfo);
  }

  /**
   * @return Combo box for selecting a wiki.
   */
  public JComboBox<EnumWikipedia> getSelector() {
    return combo;
  }

  /**
   * @return Label to be associated with the combo box.
   */
  public JLabel getLabel() {
    return label;
  }

  /**
   * @return Button to be associated with the combo box.
   */
  public JComponent getTools() {
    return toolbar;
  }

  /**
   * Add a listener to the list of change listeners.
   * 
   * @param listener Listener to be added to the list of change listeners.
   */
  public void addChangeListener(WikiChangeListener listener) {
    if ((listener != null) && (!changeListeners.contains(listener))) {
      changeListeners.add(listener);
    }
  }

  /**
   * Notify listeners of a change in the wiki selection.
   */
  public void notifyWikiChange() {
    EnumWikipedia wiki = getWiki();
    for (WikiChangeListener listener : changeListeners) {
      listener.changeWiki(wiki);
    }
  }

  /**
   * Action called when Other Wiki button is pressed.
   */
  public void actionOtherWiki() {
    String url = URL_OTHER_WIKI;
    if (Utilities.isDesktopSupported()) {
      Utilities.browseURL(url);
    } else {
      Utilities.displayUrlMessage(
          parentComponent,
          GT._T("You can learn how to add other Wikipedia at the following URL:"),
          url);
    }
  }

  /**
   * Retrieve the currently selected wiki.
   * 
   * @return Selected wiki.
   * @see org.wikipediacleaner.api.dataaccess.WikiProvider#getWiki()
   */
  @Override
  public EnumWikipedia getWiki() {
    if (combo == null) {
      return null;
    }
    return (EnumWikipedia) combo.getSelectedItem();
  }
}

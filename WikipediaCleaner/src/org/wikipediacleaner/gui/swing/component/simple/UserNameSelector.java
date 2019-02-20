/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2019  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.gui.swing.component.simple;

import java.awt.Component;
import java.awt.event.ItemListener;
import java.beans.EventHandler;
import java.util.ArrayList;
import java.util.List;
import java.util.Properties;

import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.SwingConstants;

import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.gui.swing.basic.Utilities;
import org.wikipediacleaner.i18n.GT;
import org.wikipediacleaner.utils.Configuration;
import org.wikipediacleaner.utils.ConfigurationValueString;

/**
 * An helper class to build a combo box for selecting an user name.
 */
public class UserNameSelector implements WikiChangeListener {

  /** Parent component */
  @SuppressWarnings("unused")
  private final Component parentComponent;

  /** Combo box for selecting an user name */
  private final JComboBox<String> combo;

  /** Label to be associated with the combo box */
  private final JLabel label;

  /** Listener for changes in the user name selection */
  private final List<UserNameChangeListener> changeListeners;

  /**
   * Constructor.
   * 
   * @param parentComponent Parent component.
   */
  public UserNameSelector(Component parentComponent) {
    this.parentComponent = parentComponent;
    this.changeListeners = new ArrayList<>();
    
    // Create combo box
    combo = new JComboBox<>();
    combo.setEditable(true);
    combo.addItemListener(EventHandler.create(
        ItemListener.class, this, "notifyUserNameChange"));

    // Create label
    label = Utilities.createJLabel(GT._T("Username:"));
    label.setLabelFor(combo);
    label.setHorizontalAlignment(SwingConstants.TRAILING);
  }

  /**
   * @return Combo box for selecting an user name.
   */
  public JComboBox<String> getSelector() {
    return combo;
  }

  /**
   * @return Label to be associated with the combo box.
   */
  public JLabel getLabel() {
    return label;
  }

  /**
   * Add a listener to the list of change listeners.
   * 
   * @param listener Listener to be added to the list of change listeners.
   */
  public void addChangeListener(UserNameChangeListener listener) {
    if ((listener != null) && (!changeListeners.contains(listener))) {
      changeListeners.add(listener);
    }
  }

  /**
   * Notify listeners of a change in the user name selection.
   */
  public void notifyUserNameChange() {
    String userName = getUserName();
    for (UserNameChangeListener listener : changeListeners) {
      listener.changeUserName(userName);
    }
  }

  /**
   * Retrieve the currently selected user name.
   * 
   * @return Selected user name.
   */
  public String getUserName() {
    if ((combo == null) || (combo.getSelectedItem() == null)) {
      return null;
    }
    return (String) combo.getSelectedItem();
  }

  /**
   * Called when the wiki selection has changed.
   * 
   * @param wiki New wiki.
   * @see org.wikipediacleaner.gui.swing.component.simple.WikiChangeListener#changeWiki(org.wikipediacleaner.api.constants.EnumWikipedia)
   */
  @Override
  public void changeWiki(EnumWikipedia wiki) {
    combo.removeAllItems();
    combo.setSelectedItem("");
    if (wiki != null) {
      Configuration configuration = Configuration.getConfiguration();
      Properties users = configuration.getProperties(wiki, Configuration.PROPERTIES_USERS);
      for (Object user : users.keySet()) {
        combo.addItem((String) user);
      }
      if (combo.getItemCount() > 0) {
        combo.setSelectedIndex(0);
      }
      String lastUser = configuration.getString(wiki, ConfigurationValueString.LAST_USER);
      if (lastUser != null) {
        combo.setSelectedItem(lastUser);
      }
    }
    notifyUserNameChange();
  }
}

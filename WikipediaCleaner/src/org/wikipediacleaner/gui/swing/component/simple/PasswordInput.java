/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2019  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.gui.swing.component.simple;

import java.awt.Component;
import java.util.Properties;

import javax.swing.JLabel;
import javax.swing.JPasswordField;
import javax.swing.SwingConstants;

import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.dataaccess.WikiProvider;
import org.wikipediacleaner.gui.swing.basic.Utilities;
import org.wikipediacleaner.i18n.GT;
import org.wikipediacleaner.utils.Configuration;

/**
 * An helper class to build an input field for selecting a password.
 */
public class PasswordInput implements UserNameChangeListener {

  /** Parent component */
  @SuppressWarnings("unused")
  private final Component parentComponent;

  /** Provider for the wiki */
  private final WikiProvider wikiProvider;

  /** Input field for selecting a password */
  JPasswordField field;

  /** Character used for echoing password */
  private char echo = '*';

  /** Label to be associated with the input field */
  private final JLabel label;

  /**
   * Constructor.
   * 
   * @param parentComponent Parent component.
   * @param wikiProvider Wiki provider.
   */
  public PasswordInput(Component parentComponent, WikiProvider wikiProvider) {
    this.parentComponent = parentComponent;
    this.wikiProvider = wikiProvider;
    
    // Create input field
    field = new JPasswordField();
    field.setText("");
    echo = field.getEchoChar();

    // Create label
    label = Utilities.createJLabel(GT._T("Password:"));
    label.setLabelFor(field);
    label.setHorizontalAlignment(SwingConstants.TRAILING);
  }

  /**
   * @return Input field for selecting a password.
   */
  public JPasswordField getField() {
    return field;
  }

  /**
   * @return Character for echoing password.
   */
  public char getEchoChar() {
    return echo;
  }

  /**
   * @return Label to be associated with the input field.
   */
  public JLabel getLabel() {
    return label;
  }

  /**
   * Retrieve the password.
   * 
   * @return Password.
   */
  public char[] getPassword() {
    if (field == null) {
      return null;
    }
    return field.getPassword();
  }

  /**
   * Called when the user name selection has changed.
   * 
   * @param userName New user name.
   * @see org.wikipediacleaner.gui.swing.component.simple.UserNameChangeListener#changeUserName(java.lang.String)
   */
  @Override
  public void changeUserName(String userName) {
    field.setText("");
    EnumWikipedia wiki = (wikiProvider != null) ? wikiProvider.getWiki() : null;
    if ((wiki != null) && (userName != null)) {
      Configuration configuration = Configuration.getConfiguration();
      Properties users = configuration.getProperties(wiki, Configuration.PROPERTIES_USERS);
      String password = users.getProperty(userName);
      if (password != null) {
        field.setText(password);
      }
    }
  }
}

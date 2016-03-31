/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2014  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.gui.swing.worker;

import java.awt.Component;
import java.util.Properties;

import org.wikipediacleaner.api.API;
import org.wikipediacleaner.api.APIException;
import org.wikipediacleaner.api.APIFactory;
import org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithm;
import org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithm524;
import org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithms;
import org.wikipediacleaner.api.constants.EnumLanguage;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.constants.WPCConfiguration;
import org.wikipediacleaner.api.constants.WPCConfigurationBoolean;
import org.wikipediacleaner.api.data.LoginResult;
import org.wikipediacleaner.api.data.User;
import org.wikipediacleaner.gui.swing.basic.BasicWindow;
import org.wikipediacleaner.gui.swing.basic.BasicWorker;
import org.wikipediacleaner.i18n.GT;
import org.wikipediacleaner.utils.Configuration;
import org.wikipediacleaner.utils.ConfigurationConstants;
import org.wikipediacleaner.utils.ConfigurationValueInteger;
import org.wikipediacleaner.utils.ConfigurationValueString;

/**
 * SwingWorker for login. 
 */
public class LoginWorker extends BasicWorker {

  private final Component focusComponent;
  private final EnumLanguage language;
  private String username;
  private final char[] password;
  private final int saveUser;
  private final boolean login;
  private final boolean reloadOnly;
  private boolean logged;

  public LoginWorker(
      EnumWikipedia wikipedia,
      BasicWindow window, Component focusComponent,
      EnumLanguage language,
      String username,
      char[] password,
      int saveUser,
      boolean login,
      boolean reloadOnly) {
    super(wikipedia, window);
    this.focusComponent = focusComponent;
    this.language = language;
    this.username = username.trim();
    this.password = password;
    this.saveUser = saveUser;
    this.login = login;
    this.reloadOnly = reloadOnly;
    this.logged = false;
  }

  /* (non-Javadoc)
   * @see org.wikipediacleaner.gui.swing.utils.SwingWorker#finished()
   */
  @Override
  public void finished() {
    super.finished();
    if (password != null) {
      for (int i = 0; i < password.length; i++) {
        password[i] = '\0';
      }
    }
    if (focusComponent != null) {
      focusComponent.requestFocusInWindow();
    }
  }

  /* (non-Javadoc)
   * @see org.wikipediacleaner.gui.swing.utils.SwingWorker#construct()
   */
  @Override
  public Object construct() {
    try {
      setText(GT._("Retrieving MediaWiki API"));
      API api = APIFactory.getAPI();
      EnumWikipedia wiki = getWikipedia();

      // Login
      if (!reloadOnly) {
        setText(GT._("Login"));
        LoginResult result = api.login(wiki, username, new String(password), login);
        if (login) {
          if ((result == null) || (!result.isLoginSuccessful())) {
            throw new APIException("Login unsuccessful: " + ((result != null) ? result.toString() : ""));
          }
        }
        User user = api.retrieveUser(wiki, username);
        username = (user != null) ? user.getName() : null;
        wiki.getConnection().setUser(user);
        api.retrieveTokens(wiki);
        logged = true;
      }

      // Load configuration
      setText(GT._("Loading configuration"));
      api.loadConfiguration(wiki, username);

      // Saving settings
      Configuration configuration = Configuration.getConfiguration();
      configuration.setWikipedia(wiki);
      configuration.setLanguage(language);
      if (login && !reloadOnly && (saveUser != ConfigurationConstants.VALUE_SAVE_USER_NO_CHANGE)) {
        Properties props = configuration.getProperties(wiki, Configuration.PROPERTIES_USERS);
        if (saveUser == ConfigurationConstants.VALUE_SAVE_USER_NONE) {
          props.remove(username);
          configuration.setString(wiki, ConfigurationValueString.LAST_USER, (String) null);
        } else {
          props.setProperty(
              username,
              (saveUser == ConfigurationConstants.VALUE_SAVE_USER_BOTH) ? new String(password) : "");
          configuration.setString(wiki, ConfigurationValueString.LAST_USER, username);
        }
        configuration.setProperties(wiki, Configuration.PROPERTIES_USERS, props);
        configuration.setInt(
            null,
            ConfigurationValueInteger.SAVE_USER,
            saveUser);
      }
      Configuration.getConfiguration().save();
      WPCConfiguration wpcConfig = wiki.getConfiguration();

      // Retrieving disambiguation templates
      boolean useDisambig = wpcConfig.getBoolean(WPCConfigurationBoolean.DAB_USE_DISAMBIG_MAGIC_WORD);
      if (!useDisambig) {
        setText(GT._("Retrieving disambiguation templates"));
        wiki.initDisambiguationTemplates(api);
      }

      // Retrieving suggestions for text replacements
      setText(GT._("Retrieving suggestions for text replacements"));
      wpcConfig.initSuggestions(api, reloadOnly);

      // Retrieving Check Wiki configuration
      setText(GT._("Retrieving Check Wiki configuration"));
      APIFactory.getCheckWiki().retrieveConfiguration(wiki, this);

      // Retrieving special configuration
      CheckErrorAlgorithm algo524 = CheckErrorAlgorithms.getAlgorithm(wiki, 524);
      if ((algo524 != null) &&
          algo524.isAvailable() &&
          CheckErrorAlgorithms.isAlgorithmActive(wiki, 524)) {
        ((CheckErrorAlgorithm524) algo524).setTrackingCategory(api.loadMessage(wiki, "duplicate-args-category"));
      }
    } catch (APIException e) {
      return e;
    }
    return null;
  }

  /**
   * @return True if user is logged in.
   */
  public boolean isLogged() {
    return logged;
  }
}
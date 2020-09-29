/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2014  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.gui.swing.worker;

import java.awt.Component;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Properties;

import org.wikipediacleaner.api.API;
import org.wikipediacleaner.api.APIException;
import org.wikipediacleaner.api.APIFactory;
import org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithm;
import org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithm524;
import org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithm528;
import org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithm529;
import org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithm530;
import org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithms;
import org.wikipediacleaner.api.configuration.WPCConfiguration;
import org.wikipediacleaner.api.configuration.WPCConfigurationBoolean;
import org.wikipediacleaner.api.configuration.WikiConfiguration;
import org.wikipediacleaner.api.constants.EnumLanguage;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.LinterCategory;
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

  /**
   * @param wikipedia Wiki.
   * @param window Window.
   * @param focusComponent Component with the focus.
   * @param language Language.
   * @param username User name.
   * @param password Password.
   * @param saveUser True if user should be memorized.
   * @param login True if login is requested.
   * @param reloadOnly True if only a reload is requested.
   */
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
      setText(GT._T("Retrieving MediaWiki API"));
      API api = APIFactory.getAPI();
      EnumWikipedia wiki = getWikipedia();

      // Login
      if (!reloadOnly) {
        setText(GT._T("Login"));
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
      setText(GT._T("Loading configuration"));
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
        setText(GT._T("Retrieving disambiguation templates"));
        wiki.initDisambiguationTemplates(api);
      }

      // Retrieving suggestions for text replacements
      setText(GT._T("Retrieving suggestions for text replacements"));
      wpcConfig.initSuggestions(api, reloadOnly);

      // Retrieving Check Wiki configuration
      setText(GT._T("Retrieving Check Wiki configuration"));
      APIFactory.getCheckWiki().retrieveConfiguration(wiki, this);

      // Retrieving special configuration
      // TODO: Refactoring
      List<String> messageNames = new ArrayList<>();
      CheckErrorAlgorithm algo524 = CheckErrorAlgorithms.getAlgorithm(wiki, 524);
      if ((algo524 != null) &&
          algo524.isAvailable() &&
          CheckErrorAlgorithms.isAlgorithmActive(wiki, 524)) {
        messageNames.add("duplicate-args-category");
      }
      CheckErrorAlgorithm algo528 = CheckErrorAlgorithms.getAlgorithm(wiki, 528);
      if ((algo528 != null) &&
          algo528.isAvailable() &&
          CheckErrorAlgorithms.isAlgorithmActive(wiki, 528)) {
        messageNames.add("magiclink-tracking-pmid");
      }
      CheckErrorAlgorithm algo529 = CheckErrorAlgorithms.getAlgorithm(wiki, 529);
      if ((algo529 != null) &&
          algo529.isAvailable() &&
          CheckErrorAlgorithms.isAlgorithmActive(wiki, 529)) {
        messageNames.add("magiclink-tracking-isbn");
      }
      CheckErrorAlgorithm algo530 = CheckErrorAlgorithms.getAlgorithm(wiki, 530);
      if ((algo530 != null) &&
          algo530.isAvailable() &&
          CheckErrorAlgorithms.isAlgorithmActive(wiki, 530)) {
        messageNames.add("magiclink-tracking-rfc");
      }
      WikiConfiguration wikiConfig = wiki.getWikiConfiguration();
      if (wikiConfig.getLinterCategories() != null) {
        for (LinterCategory category : wikiConfig.getLinterCategories()) {
          String priority = "linter-heading-" + category.getLevel() + "-priority";
          if (!messageNames.contains(priority)) {
            messageNames.add(priority);
          }
          String cat = "linter-category-" + category.getCategory();
          if (!messageNames.contains(cat)) {
            messageNames.add(cat);
          }
        }
      }
      if (!messageNames.isEmpty()) {
        Map<String, String> messages = api.loadMessages(wiki, messageNames);
        wikiConfig.setMessages(messages);
        if ((algo524 != null) &&
            algo524.isAvailable() &&
            CheckErrorAlgorithms.isAlgorithmActive(wiki, 524)) {
          ((CheckErrorAlgorithm524) algo524).setTrackingCategory(messages.get("duplicate-args-category"));
        }
        if ((algo528 != null) &&
            algo528.isAvailable() &&
            CheckErrorAlgorithms.isAlgorithmActive(wiki, 528)) {
          ((CheckErrorAlgorithm528) algo528).setTrackingCategory(messages.get("magiclink-tracking-pmid"));
        }
        if ((algo529 != null) &&
            algo529.isAvailable() &&
            CheckErrorAlgorithms.isAlgorithmActive(wiki, 529)) {
          ((CheckErrorAlgorithm529) algo529).setTrackingCategory(messages.get("magiclink-tracking-isbn"));
        }
        if ((algo530 != null) &&
            algo530.isAvailable() &&
            CheckErrorAlgorithms.isAlgorithmActive(wiki, 530)) {
          ((CheckErrorAlgorithm530) algo530).setTrackingCategory(messages.get("magiclink-tracking-rfc"));
        }
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
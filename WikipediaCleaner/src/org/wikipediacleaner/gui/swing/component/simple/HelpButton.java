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

import org.wikipediacleaner.Version;
import org.wikipediacleaner.api.configuration.WPCConfigurationString;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.dataaccess.WikiProvider;
import org.wikipediacleaner.gui.swing.basic.Utilities;
import org.wikipediacleaner.i18n.GT;
import org.wikipediacleaner.images.EnumImageSize;
import org.wikipediacleaner.utils.ConfigurationValueShortcut;

/**
 * An helper class to build a help button.
 */
public class HelpButton {

  /** Parent component */
  private final Component parentComponent;

  /** Wiki provider */
  private final WikiProvider wikiProvider;

  /** Button for help */
  private final JButton button;

  /**
   * Constructor.
   * 
   * @param parentComponent Parent component.
   * @param wikiProvider Provider for the wiki.
   */
  public HelpButton(Component parentComponent, WikiProvider wikiProvider) {
    this.parentComponent = parentComponent;
    this.wikiProvider = wikiProvider;
    
    // Create button
    button = Utilities.createJButton(
        "tango-help-browser.png", EnumImageSize.NORMAL,
        GT._T("Help"), true,
        ConfigurationValueShortcut.HELP);
    button.addActionListener(EventHandler.create(
        ActionListener.class, this, "actionHelp"));
  }

  /**
   * @return Button for help.
   */
  public JComponent getButton() {
    return button;
  }

  /**
   * Action called when Help button is pressed.
   */
  public void actionHelp() {
    WPCConfigurationString attributeHelpURL = WPCConfigurationString.HELP_URL;
    String url = EnumWikipedia.EN.getConfiguration().getString(attributeHelpURL);
    if (wikiProvider != null) {
      EnumWikipedia wiki = wikiProvider.getWiki();
      if ((wiki != null) &&
          (wiki.getConfiguration() != null) &&
          (wiki.getConfiguration().getString(attributeHelpURL) != null)) {
        url = wiki.getConfiguration().getString(attributeHelpURL);
      }
    }
    if (Utilities.isDesktopSupported()) {
      Utilities.browseURL(url);
    } else {
      Utilities.displayUrlMessage(
          parentComponent,
          GT._T("You can read the help on {0} at the following URL:", Version.PROGRAM),
          url);
    }
  }
}

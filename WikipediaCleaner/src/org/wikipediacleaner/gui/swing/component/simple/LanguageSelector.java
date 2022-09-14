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

import org.wikipediacleaner.api.constants.EnumLanguage;
import org.wikipediacleaner.gui.swing.basic.Utilities;
import org.wikipediacleaner.i18n.GT;
import org.wikipediacleaner.images.EnumImageSize;
import org.wikipediacleaner.utils.Configuration;

/**
 * An helper class to build a combo box for selecting a language.
 */
public class LanguageSelector {

  /** URL for other languages than the defined ones */
  private final static String URL_OTHER_LANGUAGE  = "http://en.wikipedia.org/wiki/Wikipedia:WPCleaner/Languages";

  /** Parent component */
  private final Component parentComponent;

  /** Combo box for selecting a language */
  private final JComboBox<EnumLanguage> combo;

  /** Label to be associated with the combo box */
  private final JLabel label;

  /** Button to be associated with the combo box */
  private final JToolBar toolbar;

  /** Listener for changes in the language selection */
  private final List<LanguageChangeListener> changeListeners;

  /**
   * Constructor.
   * 
   * @param parentComponent Parent component.
   */
  public LanguageSelector(Component parentComponent) {
    this.parentComponent = parentComponent;
    this.changeListeners = new ArrayList<>();

    // Create combo box
    Configuration configuration = Configuration.getConfiguration();
    EnumLanguage defaultLanguage = configuration.getLanguage();
    combo = new JComboBox<>(EnumLanguage.getList().toArray(new EnumLanguage[0]));
    combo.setEditable(false);
    combo.setSelectedItem(defaultLanguage);
    combo.addItemListener(EventHandler.create(
        ItemListener.class, this, "notifyLanguageChange"));

    // Create label
    label = Utilities.createJLabel(GT._T("Language"));
    label.setLabelFor(combo);
    label.setHorizontalAlignment(SwingConstants.TRAILING);

    // Create button
    toolbar = new JToolBar(SwingConstants.HORIZONTAL);
    toolbar.setFloatable(false);
    toolbar.setBorderPainted(false);
    JButton buttonLanguageInfo = Utilities.createJButton(
        "tango-help-browser.png", EnumImageSize.SMALL,
        GT._T("Other Language"), false, null);
    buttonLanguageInfo.addActionListener(EventHandler.create(
        ActionListener.class, this, "actionOtherLanguage"));
    toolbar.add(buttonLanguageInfo);
  }

  /**
   * @return Combo box for selecting a language.
   */
  public JComboBox<EnumLanguage> getSelector() {
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
  public void addChangeListener(LanguageChangeListener listener) {
    if ((listener != null) && (!changeListeners.contains(listener))) {
      changeListeners.add(listener);
    }
  }

  /**
   * Notify listeners of a change in the language selection.
   */
  public void notifyLanguageChange() {
    EnumLanguage language = getLanguage();
    if (language != null) {
      GT.setCurrentLanguage(language);
    }
    for (LanguageChangeListener listener : changeListeners) {
      listener.changeLanguage(language);
    }
  }

  /**
   * Action called when Other Language button is pressed. 
   */
  public void actionOtherLanguage() {
    String url = URL_OTHER_LANGUAGE;
    Utilities.browseURL(url, () -> Utilities.displayUrlMessage(
        parentComponent,
        GT._T("You can learn how to add other languages at the following URL:"),
        url));
  }

  /**
   * @return Selected language.
   */
  public EnumLanguage getLanguage() {
    if (combo == null) {
      return null;
    }
    return (EnumLanguage) combo.getSelectedItem();
  }
}

/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.gui.swing;

import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionListener;
import java.beans.EventHandler;
import java.util.List;
import java.util.Vector;

import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.JPanel;
import javax.swing.JPopupMenu;
import javax.swing.JTextField;
import javax.swing.SwingConstants;

import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.constants.WPCConfiguration;
import org.wikipediacleaner.api.constants.WPCConfigurationStringList;
import org.wikipediacleaner.api.data.LanguageRegistry;
import org.wikipediacleaner.gui.swing.basic.Utilities;
import org.wikipediacleaner.i18n.GT;
import org.wikipediacleaner.utils.Configuration;
import org.wikipediacleaner.utils.ConfigurationValueString;


/**
 * A panel for language selection.
 */
public class LanguageSelectionPanel extends JPanel {

  /**
   * Serialization.
   */
  private static final long serialVersionUID = -3237387577277476620L;

  /**
   * Wiki.
   */
  private final EnumWikipedia wiki;

  /**
   * Text in foreign language.
   */
  private final String text;

  /**
   * Language registry.
   */
  private LanguageRegistry registry;

  /**
   * Button for selecting the language.
   */
  private JButton buttonLanguage;

  /**
   * Text for the selected language.
   */
  private JTextField txtLanguage;

  /**
   * Selected language.
   */
  private LanguageRegistry.Language language;

  /**
   * Button for selecting the script.
   */
  private JButton buttonScript;

  /**
   * Text for the selected script.
   */
  private JTextField txtScript;

  /**
   * Selected script.
   */
  private LanguageRegistry.Script script;

  /**
   * Button for selecting the region.
   */
  private JButton buttonRegion;

  /**
   * Text for the selected region.
   */
  private JTextField txtRegion;

  /**
   * Selected region.
   */
  private LanguageRegistry.Region region;

  private final static int NB_VARIANTS = 4;

  /**
   * Buttons for selecting the variants.
   */
  private Vector<JButton> buttonVariant;

  /**
   * Texts for the selected variants.
   */
  private Vector<JTextField> txtVariant;

  /**
   * Selected variants.
   */
  private Vector<LanguageRegistry.Variant> variant;

  /**
   * Create a language selection panel.
   * 
   * @param wiki Wiki.
   * @param text Text in foreign language.
   */
  public LanguageSelectionPanel(EnumWikipedia wiki, String text) {
    super(new GridBagLayout(), true);
    this.wiki = wiki;
    this.text = text;
    registry = new LanguageRegistry();
    constructContents();
  }

  /**
   * @return Language string selected by the user.
   */
  public String getLang() {
    if (language == null) {
      return null;
    }
    return language.getCode();
  }

  /**
   * @return Full language string selected by the user.
   */
  public String getLanguage() {

    // Language component
    if (language == null) {
      return null;
    }
    String result = language.getCode();

    // Script component
    if (script != null) {
      result += "-" + script.getCode();
    }

    // Region component
    if (region != null) {
      result += "-" + region.getCode();
    }

    return result;
  }

  /**
   * Construct panel contents.
   */
  private void constructContents() {
    GridBagConstraints constraints = new GridBagConstraints(
        0, 0, 1, 1, 1, 0,
        GridBagConstraints.LINE_START, GridBagConstraints.BOTH,
        new Insets(0, 0, 0, 0), 0, 0);

    // Text
    if ((text != null) && (text.trim().length() > 0)) {
      JLabel label = new JLabel(text, SwingConstants.LEFT);
      constraints.gridwidth = 2;
      add(label, constraints);
      constraints.weightx = 1;
      constraints.gridwidth = 1;
      constraints.gridy++;
    }

    // Language
    buttonLanguage = Utilities.createJButton(GT._T("Language"), null);
    buttonLanguage.addActionListener(EventHandler.create(
        ActionListener.class, this, "actionLanguage"));
    constraints.gridx = 0;
    constraints.weightx = 0;
    add(buttonLanguage, constraints);
    txtLanguage = new JTextField("", 40);
    txtLanguage.setEditable(false);
    constraints.gridx++;
    constraints.weightx = 1;
    add(txtLanguage, constraints);
    constraints.gridy++;

    // Script
    buttonScript = Utilities.createJButton(GT._T("Script"), null);
    buttonScript.addActionListener(EventHandler.create(
        ActionListener.class, this, "actionScript"));
    buttonScript.setEnabled(false);
    constraints.gridx = 0;
    constraints.weightx = 0;
    add(buttonScript, constraints);
    txtScript = new JTextField("", 40);
    txtScript.setEditable(false);
    constraints.gridx++;
    constraints.weightx = 1;
    add(txtScript, constraints);
    constraints.gridy++;

    // Region
    buttonRegion = Utilities.createJButton(GT._T("Region"), null);
    buttonRegion.addActionListener(
        EventHandler.create(ActionListener.class, this, "actionRegion"));
    buttonRegion.setEnabled(false);
    constraints.gridx = 0;
    constraints.weightx = 0;
    add(buttonRegion, constraints);
    txtRegion = new JTextField("", 40);
    txtRegion.setEditable(false);
    constraints.gridx++;
    constraints.weightx = 1;
    add(txtRegion, constraints);
    constraints.gridy++;

    // Variants
    buttonVariant = new Vector<JButton>(NB_VARIANTS);
    txtVariant = new Vector<JTextField>(NB_VARIANTS);
    variant = new Vector<LanguageRegistry.Variant>(NB_VARIANTS);
    for (int i = 0; i < NB_VARIANTS; i++) {
      JButton tmpButton = Utilities.createJButton(GT._T("Variant n°{0}", Integer.toString(i + 1)), null);
      tmpButton.setActionCommand(Integer.toString(i));
      tmpButton.addActionListener(EventHandler.create(
          ActionListener.class, this, "actionVariant", "actionCommand"));
      tmpButton.setEnabled(false);
      buttonVariant.add(tmpButton);
      constraints.gridx = 0;
      constraints.weightx = 0;
      add(tmpButton, constraints);
      JTextField tmpText = new JTextField("", 40);
      tmpText.setEditable(false);
      txtVariant.add(tmpText);
      constraints.gridx++;
      constraints.weightx = 1;
      add(tmpText, constraints);
      constraints.gridy++;
      variant.add(null);
    }

    // Default language
    Configuration config = Configuration.getConfiguration();
    String defaultLanguage = config.getString(null, ConfigurationValueString.LAST_LANGUAGE);
    if (defaultLanguage != null) {
      selectLanguage(defaultLanguage);
    }
  }

  /**
   * Action called when the Language button is clicked.
   */
  public void actionLanguage() {
    JPopupMenu menu = new JPopupMenu();

    // Common languages
    WPCConfiguration config = wiki.getConfiguration();
    List<String> commonLanguages = config.getStringList(WPCConfigurationStringList.COMMON_LANGUAGES);
    if ((commonLanguages != null) && (commonLanguages.size() > 0)) {
      for (String commonLanguage : commonLanguages) {
        LanguageRegistry.Language tmpLanguage = registry.getLanguage(commonLanguage);
        if (tmpLanguage != null) {
          JMenuItem item = new JMenuItem(tmpLanguage.toString());
          item.setActionCommand(tmpLanguage.getCode());
          item.addActionListener(EventHandler.create(
              ActionListener.class, this, "selectLanguage", "actionCommand"));
          menu.add(item);
        }
      }
    }

    // All languages
    List<LanguageRegistry.Language> languages = registry.getLanguages();
    char firstLetter = '\0';
    JMenu firstMenu = null;
    char secondLetter = '\0';
    JMenu secondMenu = null;
    for (LanguageRegistry.Language lang : languages) {
      String code = lang.getCode();
      if ((firstMenu == null) || (code.charAt(0) != firstLetter)) {
        firstMenu = new JMenu(code.substring(0, 1));
        menu.add(firstMenu);
        firstLetter = code.charAt(0);
        secondLetter = '\0';
      }
      if ((secondMenu == null) || (code.charAt(1) != secondLetter)) {
        secondMenu = new JMenu(code.substring(0, 2));
        firstMenu.add(secondMenu);
        secondLetter = code.charAt(1);
      }
      JMenuItem item = new JMenuItem(lang.toString());
      item.setActionCommand(lang.getCode());
      item.addActionListener(EventHandler.create(
          ActionListener.class, this, "selectLanguage", "actionCommand"));
      secondMenu.add(item);
    }
    menu.show(buttonLanguage, 0, buttonLanguage.getHeight());
  }

  /**
   * Action called when a language is selected.
   * 
   * @param languageCode Language code.
   */
  public void selectLanguage(String languageCode) {
    language = registry.getLanguage(languageCode);
    txtLanguage.setText(language != null ? language.toString() : "");
    buttonScript.setEnabled(language != null);
    selectScript(null);
    buttonRegion.setEnabled(language != null);
    buttonVariant.get(0).setEnabled(language != null);
    for (int i = 1; i < NB_VARIANTS; i++) {
      buttonVariant.get(i).setEnabled(false);
    }
  }

  /**
   * Action called when the Script button is clicked.
   */
  public void actionScript() {
    JPopupMenu menu = new JPopupMenu();
    List<LanguageRegistry.Script> scripts = registry.getScripts(language);
    JMenu firstMenu = null;
    int count = 0;
    for (LanguageRegistry.Script tmpScript : scripts) {
      if ((firstMenu == null) || (count >= 20)) {
        firstMenu = new JMenu(tmpScript.getCode() + "...");
        menu.add(firstMenu);
        count = 0;
      }
      JMenuItem item = new JMenuItem(tmpScript.toString());
      item.setActionCommand(tmpScript.getCode());
      item.addActionListener(EventHandler.create(
          ActionListener.class, this, "selectScript", "actionCommand"));
      firstMenu.add(item);
      count++;
    }
    menu.show(buttonScript, 0, buttonScript.getHeight());
  }

  /**
   * Action called when a script is selected.
   * 
   * @param scriptCode Script code.
   */
  public void selectScript(String scriptCode) {
    script = registry.getScript(scriptCode);
    txtScript.setText(script != null ? script.toString() : "");
    selectRegion(null);
  }

  /**
   * Action called when the Region button is clicked.
   */
  public void actionRegion() {
    JPopupMenu menu = new JPopupMenu();
    List<LanguageRegistry.Region> regions = registry.getRegions();
    JMenu firstMenu = null;
    int count = 0;
    for (LanguageRegistry.Region tmpRegion : regions) {
      if ((firstMenu == null) || (count >= 20)) {
        firstMenu = new JMenu(tmpRegion.getCode() + "...");
        menu.add(firstMenu);
        count = 0;
      }
      JMenuItem item = new JMenuItem(tmpRegion.toString());
      item.setActionCommand(tmpRegion.getCode());
      item.addActionListener(EventHandler.create(
          ActionListener.class, this, "selectRegion", "actionCommand"));
      firstMenu.add(item);
      count++;
    }
    menu.show(buttonRegion, 0, buttonRegion.getHeight());
  }

  /**
   * Action called when a region is selected.
   * 
   * @param regionCode Region code.
   */
  public void selectRegion(String regionCode) {
    region = registry.getRegion(regionCode);
    txtRegion.setText(region != null ? region.toString() : "");
    selectVariant("0");
  }

  /**
   * Action called when a Variant button is clicked.
   * 
   * @param number Variant number.
   */
  public void actionVariant(String number) {
    JPopupMenu menu = new JPopupMenu();
    int variantNumber = Integer.parseInt(number);
    String prefix = language.getCode();
    if (script != null) {
      prefix += "-" + script.getCode();
    }
    if (region != null) {
      prefix += "-" + region.getCode();
    }
    for (int i = 0; i < variantNumber; i++) {
      if (variant.get(i) != null) {
        prefix += "-" + variant.get(i).getCode();
      }
    }
    List<LanguageRegistry.Variant> variants = registry.getVariants(prefix);
    for (LanguageRegistry.Variant tmpVariant : variants) {
      JMenuItem item = new JMenuItem(tmpVariant.toString());
      item.setActionCommand(number + ";" + tmpVariant.getCode());
      item.addActionListener(EventHandler.create(
          ActionListener.class, this, "selectVariant", "actionCommand"));
      menu.add(item);
    }
    menu.show(buttonVariant.get(variantNumber), 0, buttonVariant.get(variantNumber).getHeight());
  }

  /**
   * Action called when a variant is selected.
   * 
   * @param variantCode Variant number and variant code.
   */
  public void selectVariant(String variantCode) {
    if (variantCode == null) {
      return;
    }
    String[] variantElements = variantCode.split(";");
    int variantNumber = Integer.parseInt(variantElements[0]);
    if ((variantNumber < 0) || (variantNumber >= NB_VARIANTS)) {
      return;
    }
    String code = (variantElements.length > 1) ? variantElements[1] : null;
    variant.set(variantNumber, registry.getVariant(code));
    txtVariant.get(variantNumber).setText(variant.get(variantNumber) != null ? variant.get(variantNumber).toString() : "");
    for (int i = 0; i < NB_VARIANTS; i++) {
      if (i == 0) {
        buttonVariant.get(0).setEnabled(language != null);
      } else {
        buttonVariant.get(i).setEnabled(variant.get(i - 1) != null);
        if (variant.get(i - 1) == null) {
          variant.set(i, null);
          txtVariant.get(i).setText("");
        }
      }
    }
  }
}

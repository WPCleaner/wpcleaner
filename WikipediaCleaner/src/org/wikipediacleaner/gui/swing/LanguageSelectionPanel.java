/*
 *  WikipediaCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package org.wikipediacleaner.gui.swing;

import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionListener;
import java.beans.EventHandler;
import java.util.List;

import javax.swing.JButton;
import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.JPanel;
import javax.swing.JPopupMenu;
import javax.swing.JTextField;

import org.wikipediacleaner.api.data.LanguageRegistry;
import org.wikipediacleaner.gui.swing.basic.Utilities;
import org.wikipediacleaner.i18n.GT;


/**
 * A panel for language selection.
 */
public class LanguageSelectionPanel extends JPanel {

  /**
   * Serialization.
   */
  private static final long serialVersionUID = -3237387577277476620L;

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
   * Create a language selection panel.
   */
  public LanguageSelectionPanel() {
    super(new GridBagLayout(), true);
    registry = new LanguageRegistry();
    constructContents();
  }

  /**
   * @return Language string selected by the user.
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

    // Language
    buttonLanguage = Utilities.createJButton(GT._("Language"));
    buttonLanguage.addActionListener(EventHandler.create(ActionListener.class, this, "actionLanguage"));
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
    buttonScript = Utilities.createJButton(GT._("Script"));
    buttonScript.addActionListener(EventHandler.create(ActionListener.class, this, "actionScript"));
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
  }

  /**
   * Action called when the Language button is clicked.
   */
  public void actionLanguage() {
    JPopupMenu menu = new JPopupMenu();
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
      item.addActionListener(EventHandler.create(ActionListener.class, this, "selectLanguage", "actionCommand"));
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
  }

  /**
   * Action called when the Script button is clicked.
   */
  public void actionScript() {
    JPopupMenu menu = new JPopupMenu();
    List<LanguageRegistry.Script> scripts = registry.getScripts(language);
    for (LanguageRegistry.Script tmpScript : scripts) {
      JMenuItem item = new JMenuItem(tmpScript.toString());
      item.setActionCommand(tmpScript.getCode());
      item.addActionListener(EventHandler.create(ActionListener.class, this, "selectScript", "actionCommand"));
      menu.add(item);
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
  }
}

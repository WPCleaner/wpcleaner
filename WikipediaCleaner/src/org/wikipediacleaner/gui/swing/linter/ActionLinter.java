/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.gui.swing.linter;

import java.awt.Component;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.Iterator;
import java.util.List;

import javax.swing.AbstractAction;
import javax.swing.JButton;
import javax.swing.JOptionPane;
import javax.swing.JToolBar;
import javax.swing.text.JTextComponent;

import org.wikipediacleaner.api.APIException;
import org.wikipediacleaner.api.APIFactory;
import org.wikipediacleaner.api.MediaWikiRESTAPI;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.LinterCategory;
import org.wikipediacleaner.api.linter.LinterError;
import org.wikipediacleaner.gui.swing.basic.Utilities;
import org.wikipediacleaner.i18n.GT;
import org.wikipediacleaner.images.EnumImageSize;


/**
 * Manage actions for checking an article with Linter.
 */
public class ActionLinter extends AbstractAction implements ActionListener {

  /** Serialization */
  private static final long serialVersionUID = 9098138148697135308L;

  /**
   * @param showIcon True if the button should use an icon.
   * @return Button.
   */
  private static JButton createInternalButton(
      boolean showIcon) {
    return Utilities.createJButton(
        showIcon ? "Linter_logo_v2.png" : null,
        EnumImageSize.NORMAL,
        GT._("Check article with Linter"), !showIcon,
        null);
  }

  /**
   * Create a button for checking an article.
   * 
   * @param parent Parent component.
   * @param wiki Wiki.
   * @param title Page title.
   * @param textPane Text pane where the text is.
   * @param showIcon True if the button should use an icon.
   * @return Button.
   */
  public static JButton createButton(
      Component parent, EnumWikipedia wiki,
      String title, JTextComponent textPane,
      boolean showIcon) {
    JButton button = createInternalButton(showIcon);
    button.addActionListener(new ActionLinter(parent, wiki, title, textPane));
    return button;
  }

  /**
   * Add a button for checking an article.
   * 
   * @param parent Parent component.
   * @param toolbar Tool bar.
   * @param wiki Wiki.
   * @param title Page title.
   * @param textPane Text pane where the text is.
   * @param showIcon True if the button should use an icon.
   * @return Button.
   */
  public static JButton addButton(
      Component parent, JToolBar toolbar,
      EnumWikipedia wiki,
      String title, JTextComponent textPane,
      boolean showIcon) {
    JButton button = createButton(parent, wiki, title, textPane, showIcon);
    if ((button != null) && (toolbar != null)) {
      toolbar.add(button);
    }
    return button;
  }

  /** Parent component */
  private final Component parent;

  /** Wiki */
  private final EnumWikipedia wiki;

  /** Page title */
  private final String title;

  /** Text pane where the text is */
  private final JTextComponent textPane;

  /**
   * @param parent Parent component.
   * @param wiki Wiki.
   * @param textPane Text pane where the text is.
   */
  private ActionLinter(
      Component parent, EnumWikipedia wiki,
      String title, JTextComponent textPane) {
    this.parent = parent;
    this.wiki = wiki;
    this.title = title;
    this.textPane = textPane;
  }

  /**
   * Check an article.
   * 
   * @param e Event triggering this call.
   * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
   */
  @Override
  public void actionPerformed(ActionEvent e) {
    MediaWikiRESTAPI api = APIFactory.getRESTAPI();
    try {
      List<LinterError> errors = api.transformWikitextToLint(wiki, title, textPane.getText());
      if (errors == null) {
        Utilities.displayWarning(
            parent,
            GT._("Unable to retrieve analysis from Linter."));
        return;
      }
      Iterator<LinterError> itErrors = errors.iterator();
      while (itErrors.hasNext()) {
        LinterError error = itErrors.next();
        boolean found = false;
        List<LinterCategory> categories = wiki.getWikiConfiguration().getLinterCategories();
        if (categories != null) {
          for (LinterCategory category : wiki.getWikiConfiguration().getLinterCategories()) {
            if (category.getCategory().equals(error.getType())) {
              found = true;
            }
          }
        }
        if (!found) {
          itErrors.remove();
        }
      }
      if (errors.isEmpty()) {
        Utilities.displayInformationMessage(
            parent,
            GT._("No errors are currently detected by Linter."));
        return;
      }
      LinterErrorPanel panel = new LinterErrorPanel(wiki.getWikiConfiguration(), errors, textPane);
      JOptionPane.showMessageDialog(
          parent, panel, GT._("Errors"),
          JOptionPane.INFORMATION_MESSAGE);
    } catch (APIException exception) {
      Utilities.displayError(parent, exception);
      return;
    }
  }
}

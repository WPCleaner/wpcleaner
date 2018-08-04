/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.gui.swing.action;

import java.awt.Component;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JButton;
import javax.swing.JToolBar;

import org.wikipediacleaner.api.API;
import org.wikipediacleaner.api.APIException;
import org.wikipediacleaner.api.APIFactory;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.dataaccess.PageProvider;
import org.wikipediacleaner.gui.swing.basic.Utilities;
import org.wikipediacleaner.i18n.GT;
import org.wikipediacleaner.images.EnumImageSize;


/**
 * Manage actions for deleting a page.
 */
public class ActionDeletePage implements ActionListener {

  /**
   * @param showIcon True if the button should use an icon.
   * @return Button.
   */
  private static JButton createInternalButton(
      boolean showIcon) {
    String iconName = "gnome-edit-delete.png";
    String label = GT._T("Delete page");
    return Utilities.createJButton(
        showIcon ? iconName : null,
        EnumImageSize.NORMAL,
        label, !showIcon,
        null);
  }

  /**
   * Create a button for deleting a page.
   * 
   * @param parent Parent component.
   * @param pageProvider Page provider.
   * @param listener Listener.
   * @param showIcon True if the button should use an icon.
   * @return Button.
   */
  public static JButton createButton(
      Component parent, PageProvider pageProvider,
      ListenerPageDeletion listener,
      boolean showIcon) {
    JButton button = createInternalButton(showIcon);
    button.addActionListener(new ActionDeletePage(parent, pageProvider, listener));
    return button;
  }

  /**
   * Add a button for deleting a page.
   * 
   * @param toolbar Tool bar.
   * @param parent Parent component.
   * @param pageProvider Page provider.
   * @param listener Listener.
   * @param showIcon True if the button should use an icon.
   * @return Button.
   */
  public static JButton addButton(
      JToolBar toolbar,
      Component parent, PageProvider pageProvider,
      ListenerPageDeletion listener,
      boolean showIcon) {
    JButton button = createButton(parent, pageProvider, listener, showIcon);
    if ((button != null) && (toolbar != null)) {
      toolbar.add(button);
    }
    return button;
  }

  /** Parent component. */
  private final Component parent;

  /** Page provider. */
  private final PageProvider pageProvider;

  /** Listener. */
  private final ListenerPageDeletion listener;

  /**
   * @param parent Parent component.
   * @param pageProvider Page provider.
   * @param listener Listener.
   */
  private ActionDeletePage(
      Component parent,
      PageProvider pageProvider,
      ListenerPageDeletion listener) {
    this.parent = parent;
    this.pageProvider = pageProvider;
    this.listener = listener;
  }

  /**
   * Delete a page.
   * 
   * @param e Event triggering this call.
   * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
   */
  @Override
  public void actionPerformed(ActionEvent e) {
    if (pageProvider == null) {
      return;
    }
    Page page = pageProvider.getPage();
    if (page == null) {
      return;
    }

    String reason = Utilities.askForValue(
        parent,
        GT._T("Do you want to delete this page on Wikipedia ?\nPlease, enter the reason for deleting the page"),
        "", null);
    if ((reason == null) || (reason.trim().length() == 0)) {
      return;
    }
    API api = APIFactory.getAPI();
    try {
      EnumWikipedia wiki = page.getWikipedia();
      api.deletePage(
          wiki, page, reason.trim(), false);
      if (listener != null) {
        listener.pageDeleted(page.getTitle());
      }
    } catch (APIException ex) {
      Utilities.displayError(parent, ex);
    }
  }
}

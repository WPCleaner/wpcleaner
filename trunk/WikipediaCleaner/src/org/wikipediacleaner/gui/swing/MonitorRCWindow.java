/*
 *  WikipediaCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2012  Nicolas Vervelle
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

import java.awt.Component;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionListener;
import java.beans.EventHandler;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import javax.swing.JButton;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.ScrollPaneConstants;
import javax.swing.WindowConstants;

import org.wikipediacleaner.api.API;
import org.wikipediacleaner.api.APIException;
import org.wikipediacleaner.api.APIFactory;
import org.wikipediacleaner.api.RecentChangesListener;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.DataManager;
import org.wikipediacleaner.api.data.Namespace;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.data.RecentChange;
import org.wikipediacleaner.gui.swing.basic.BasicWindow;
import org.wikipediacleaner.gui.swing.basic.Utilities;
import org.wikipediacleaner.gui.swing.worker.UpdateDabWarningTools;
import org.wikipediacleaner.i18n.GT;


/**
 * A window for monitoring Recent Changes.
 */
public class MonitorRCWindow extends BasicWindow implements RecentChangesListener {

  /**
   * Table model for the list of recent changes.
   */
  private RecentChangesTableModel modelRC;

  /**
   * Table for the list of recent changes.
   */
  private JTable tableRC;

  /**
   * Table model for the list of recent changes that are interesting.
   */
  private RecentChangesTableModel modelRCInteresting;

  /**
   * Table for the list of recent changes.
   */
  private JTable tableRCInteresting;

  /**
   * Tools for updating disambiguation warning.
   */
  private UpdateDabWarningTools dabWarningTools;

  /**
   * Create and display a window for monitoring recent changes.
   * 
   * @param wiki Wiki.
   */
  public static void createMonitorRCWindow(
      final EnumWikipedia wiki) {
    createWindow(
        "MonitorRCWindow", wiki,
        WindowConstants.DISPOSE_ON_CLOSE,
        MonitorRCWindow.class, null);
  }

  /**
   * @return Title of the window.
   * @see org.wikipediacleaner.gui.swing.basic.BasicWindow#getTitle()
   */
  @Override
  public String getTitle() {
    return GT._("Recent changes monitoring");
  }

  /**
   * @return Window components.
   */
  @Override
  protected Component createComponents() {
    JPanel panel = new JPanel(new GridBagLayout());

    // Initialize constraints
    GridBagConstraints constraints = new GridBagConstraints();
    constraints.fill = GridBagConstraints.HORIZONTAL;
    constraints.gridheight = 1;
    constraints.gridwidth = 1;
    constraints.gridx = 0;
    constraints.gridy = 0;
    constraints.insets = new Insets(0, 0, 0, 0);
    constraints.ipadx = 0;
    constraints.ipady = 0;
    constraints.weightx = 0;
    constraints.weighty = 0;

    // Recent changes table
    constraints.fill = GridBagConstraints.BOTH;
    constraints.weightx = 1;
    constraints.weighty = 1;
    modelRC = new RecentChangesTableModel(null);
    tableRC = new JTable(modelRC);
    modelRC.configureColumnModel(tableRC.getColumnModel());
    JScrollPane scrollRC = new JScrollPane(tableRC);
    scrollRC.setMinimumSize(new Dimension(300, 200));
    scrollRC.setPreferredSize(new Dimension(800, 300));
    scrollRC.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_ALWAYS);
    panel.add(scrollRC, constraints);
    constraints.gridy++;

    // Interesting recent changes table
    constraints.fill = GridBagConstraints.BOTH;
    constraints.weightx = 1;
    constraints.weighty = 1;
    modelRCInteresting = new RecentChangesTableModel(null);
    tableRCInteresting = new JTable(modelRCInteresting);
    modelRCInteresting.configureColumnModel(tableRCInteresting.getColumnModel());
    JScrollPane scrollRCInteresting = new JScrollPane(tableRCInteresting);
    scrollRCInteresting.setMinimumSize(new Dimension(300, 200));
    scrollRCInteresting.setPreferredSize(new Dimension(800, 300));
    scrollRCInteresting.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_ALWAYS);
    panel.add(scrollRCInteresting, constraints);
    constraints.gridy++;

    // Buttons
    JPanel buttonPanel = new JPanel(new FlowLayout(FlowLayout.RIGHT));
    JButton buttonClose = Utilities.createJButton(GT._("&Close"));
    buttonClose.addActionListener(EventHandler.create(
        ActionListener.class, this, "dispose"));
    buttonPanel.add(buttonClose);
    constraints.fill = GridBagConstraints.HORIZONTAL;
    constraints.gridx = 0;
    constraints.weightx = 1;
    constraints.weighty = 0;
    panel.add(buttonPanel, constraints);
    constraints.gridy++;

    updateComponentState();
    dabWarningTools = new UpdateDabWarningTools(getWikipedia(), this);
    API api = APIFactory.getAPI();
    api.addRecentChangesListener(getWikipedia(), this);
    return panel;
  }

  /**
   * Dispose window.

   * @see org.wikipediacleaner.gui.swing.basic.BasicWindow#dispose()
   */
  @Override
  public void dispose() {
    API api = APIFactory.getAPI();
    api.removeRecentChangesListener(getWikipedia(), this);
    super.dispose();
  }

  /**
   * Callback to be notified about recent changes.
   * 
   * @param newRC List of recent changes.
   * @param currentTime Current time.
   * @see org.wikipediacleaner.api.RecentChangesListener#recentChanges(java.util.List, java.util.Date)
   */
  public void recentChanges(List<RecentChange> newRC, Date currentTime) {
    modelRC.addRecentChanges(newRC);

    // Update list of interesting recent changes
    for (RecentChange rc : newRC) {
      if (isInterestingNamespace(rc)) {
        if (RecentChange.TYPE_NEW.equals(rc.getType())) {
          if (rc.isNew()) {
            modelRCInteresting.addRecentChange(rc);
          }
        } else if (RecentChange.TYPE_EDIT.equals(rc.getType())) {
          if (modelRCInteresting.containsRecentChange(rc.getTitle())) {
            modelRCInteresting.addRecentChange(rc);
          }
        } else if (RecentChange.TYPE_LOG.equals(rc.getType())) {
          if (RecentChange.LOG_TYPE_DELETE.equals(rc.getLogType()) &&
              RecentChange.LOG_ACTION_DELETE_DELETE.equals(rc.getLogAction())) {
            modelRCInteresting.removeRecentChanges(rc.getTitle());
          }
        }
      }
    }

    // Check if interesting recent changes are old enough
    List<RecentChange> interestingRC = modelRCInteresting.getRecentChanges();
    List<Page> pages = new ArrayList<Page>();
    Map<String, String> creators = new HashMap<String, String>();
    Map<String, List<String>> modifiers = new HashMap<String, List<String>>();
    while (!interestingRC.isEmpty()) {

      // Retrieve synthetic information about recent changes for one title
      List<RecentChange> listRC = extractRecentChanges(interestingRC);
      String title = listRC.get(0).getTitle();
      String creator = null;
      List<String> pageModifiers = new ArrayList<String>();
      boolean oldEnough = true;
      for (RecentChange rc : listRC) {
        if (currentTime.getTime() <= rc.getTimestamp().getTime() + 15*60*1000) {
          oldEnough = false;
        }
        String user = rc.getUser();
        if (rc.isNew()) {
          creator = user;
        } else {
          if (!rc.isBot()) {
            if ((creator == null) || (!creator.equals(user))) {
              if (!pageModifiers.contains(user)) {
                pageModifiers.add(user);
              }
            }
          }
        }
      }

      if (oldEnough) {
        modelRCInteresting.removeRecentChanges(title);
        Page page = DataManager.getPage(getWikipedia(), title, null, null);
        pages.add(page);
        creators.put(title, creator);
        modifiers.put(title, pageModifiers);
      }
    }

    // Update disambiguation warnings
    if (!pages.isEmpty()) {
      try {
        dabWarningTools.updateDabWarning(pages, false, false, false);
      } catch (APIException e) {
        // Nothing to do
      }
    }
  }

  /**
   * Extract a list of recent changes for the same page.
   * 
   * @param allRC List of recent changes.
   * @return List of recent changes for one page.
   */
  private List<RecentChange> extractRecentChanges(List<RecentChange> allRC) {
    if ((allRC == null) || (allRC.isEmpty())) {
      return null;
    }
    List<RecentChange> result = new ArrayList<RecentChange>();
    String title = allRC.get(0).getTitle();
    Iterator<RecentChange> itRC = allRC.iterator();
    while (itRC.hasNext()) {
      RecentChange rc = itRC.next();
      if (Page.areSameTitle(title, rc.getTitle())) {
        result.add(rc);
        itRC.remove();
      }
    }
    return result;
  }

  /**
   * @param rc Recent change.
   * @return True if the recent change is in an interesting namespace.
   */
  private boolean isInterestingNamespace(RecentChange rc) {
    if (rc == null) {
      return false;
    }
    int namespace = rc.getNamespace();
    if ((namespace == Namespace.MAIN) ||
        (namespace == Namespace.TEMPLATE) ||
        (namespace == Namespace.CATEGORY)) {
      return true;
    }
    return false;
  }
}

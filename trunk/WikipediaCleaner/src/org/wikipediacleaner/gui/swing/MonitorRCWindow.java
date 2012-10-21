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
import java.util.Date;
import java.util.List;

import javax.swing.JButton;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.ScrollPaneConstants;
import javax.swing.WindowConstants;
import javax.swing.table.TableColumnModel;

import org.wikipediacleaner.api.API;
import org.wikipediacleaner.api.APIFactory;
import org.wikipediacleaner.api.RecentChangesListener;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.RecentChange;
import org.wikipediacleaner.gui.swing.basic.BasicWindow;
import org.wikipediacleaner.gui.swing.basic.Utilities;
import org.wikipediacleaner.i18n.GT;


/**
 * A window for monitoring Recent Changes.
 */
public class MonitorRCWindow extends BasicWindow implements RecentChangesListener {

  RecentChangesTableModel modelRC;
  JTable tableRC;

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
    TableColumnModel columnModel = tableRC.getColumnModel();
    columnModel.getColumn(RecentChangesTableModel.COLUMN_FLAGS).setMinWidth(40);
    columnModel.getColumn(RecentChangesTableModel.COLUMN_FLAGS).setMaxWidth(40);
    columnModel.getColumn(RecentChangesTableModel.COLUMN_RC_ID).setMinWidth(80);
    columnModel.getColumn(RecentChangesTableModel.COLUMN_RC_ID).setPreferredWidth(80);
    columnModel.getColumn(RecentChangesTableModel.COLUMN_RC_ID).setMaxWidth(100);
    columnModel.getColumn(RecentChangesTableModel.COLUMN_TITLE).setMinWidth(100);
    columnModel.getColumn(RecentChangesTableModel.COLUMN_TITLE).setPreferredWidth(200);
    JScrollPane scrollRC = new JScrollPane(tableRC);
    scrollRC.setMinimumSize(new Dimension(300, 200));
    scrollRC.setPreferredSize(new Dimension(800, 400));
    scrollRC.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_ALWAYS);
    panel.add(scrollRC, constraints);
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
   * @param rc List of recent changes.
   * @param currentTime Current time.
   * @see org.wikipediacleaner.api.RecentChangesListener#recentChanges(java.util.List, java.util.Date)
   */
  public void recentChanges(List<RecentChange> rc, Date currentTime) {
    modelRC.addRecentChanges(rc);
  }
}

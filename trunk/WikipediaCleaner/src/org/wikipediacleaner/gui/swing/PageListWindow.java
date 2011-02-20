/*
 *  WikipediaCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2007  Nicolas Vervelle
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
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.JToolBar;
import javax.swing.ScrollPaneConstants;
import javax.swing.SwingConstants;
import javax.swing.WindowConstants;
import javax.swing.table.TableColumnModel;

import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.DataManager;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.data.PageComment;
import org.wikipediacleaner.api.data.ProgressionValue;
import org.wikipediacleaner.gui.swing.basic.BasicWindow;
import org.wikipediacleaner.gui.swing.basic.BasicWorker;
import org.wikipediacleaner.gui.swing.basic.DefaultBasicWindowListener;
import org.wikipediacleaner.gui.swing.basic.DefaultBasicWorkerListener;
import org.wikipediacleaner.gui.swing.basic.Utilities;
import org.wikipediacleaner.gui.swing.worker.UpdateDabWarningWorker;
import org.wikipediacleaner.gui.swing.worker.UpdateInfoWorker;
import org.wikipediacleaner.i18n.GT;
import org.wikipediacleaner.images.EnumImageSize;
import org.wikipediacleaner.utils.Configuration;


/**
 * List of pages window.
 */
public class PageListWindow extends BasicWindow implements ActionListener {

  private final static String ACTION_ADD            = "ADD";
  private final static String ACTION_DISAMBIGUATION = "DISAMBIGUATION";
  private final static String ACTION_FULL_ANALYSIS  = "FULL ANALYSIS";
  private final static String ACTION_REMOVE         = "REMOVE";
  private final static String ACTION_SET_COMMENTS   = "SET COMMENTS";
  private final static String ACTION_UPDATE         = "UPDATE INFO";
  private final static String ACTION_UPDATE_DAB     = "UPDATE DAB WARNING";

  String title;
  List<Page> pages;
  boolean watchList;

  PageListTableModel modelPages;
  JTable tablePages;

  JLabel  labelLinksCount;
  
  private JButton buttonFullAnalysis;
  private JButton buttonDisambiguation;
  private JButton buttonUpdateDabWarning;
  private JButton buttonRemove;
  private JButton buttonUpdateInfo;
  private JButton buttonComments;
  private JButton buttonAdd;

  /**
   * Create and display a PageListWindow.
   * 
   * @param title Window title.
   * @param pages Pages.
   * @param wikipedia Wikipedia.
   * @param watchList Flag indicating if pages can be removed.
   */
  public static void createPageListWindow(
      final String title,
      final List<Page> pages,
      final EnumWikipedia wikipedia,
      final boolean watchList) {
    createWindow(
        "PageListWindow", wikipedia,
        WindowConstants.DISPOSE_ON_CLOSE,
        PageListWindow.class,
        new DefaultBasicWindowListener() {
          @Override
          public void initializeWindow(BasicWindow window) {
            if (window instanceof PageListWindow) {
              PageListWindow pageList = (PageListWindow) window;
              pageList.title = title;
              pageList.pages = pages;
              pageList.watchList = watchList;
            }
          }
        });
  }

  /* (non-Javadoc)
   * @see org.wikipediacleaner.gui.swing.basic.BasicWindow#getTitle()
   */
  @Override
  public String getTitle() {
    return title;
  }

  /**
   * Update component state.
   */
  @Override
  protected void updateComponentState() {
    if (tablePages != null) {
      tablePages.invalidate();
    }
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
    constraints.gridwidth = 2;
    constraints.gridx = 0;
    constraints.gridy = 0;
    constraints.insets = new Insets(0, 0, 0, 0);
    constraints.ipadx = 0;
    constraints.ipady = 0;
    constraints.weightx = 1;
    constraints.weighty = 0;

    // Table
    constraints.fill = GridBagConstraints.BOTH;
    constraints.weighty = 1;
    modelPages = new PageListTableModel(pages);
    tablePages = new JTable(modelPages);
    tablePages.setDefaultRenderer(ProgressionValue.class, new ProgressionValueCellRenderer());
    TableColumnModel columnModel = tablePages.getColumnModel();
    columnModel.getColumn(PageListTableModel.COLUMN_BACKLINKS).setMaxWidth(70);
    columnModel.getColumn(PageListTableModel.COLUMN_BACKLINKS_MAIN).setMaxWidth(70);
    columnModel.getColumn(PageListTableModel.COLUMN_BACKLINKS_TEMPLATE).setMaxWidth(70);
    columnModel.getColumn(PageListTableModel.COLUMN_COMMENTS_TEXT).setPreferredWidth(200);
    columnModel.getColumn(PageListTableModel.COLUMN_DISAMBIGUATION).setMaxWidth(40);
    columnModel.getColumn(PageListTableModel.COLUMN_REDIRECT).setMaxWidth(40);
    columnModel.getColumn(PageListTableModel.COLUMN_PAGE).setPreferredWidth(300);
    Utilities.addRowSorter(tablePages, modelPages);
    JScrollPane scrollPages = new JScrollPane(tablePages);
    scrollPages.setMinimumSize(new Dimension(100, 100));
    scrollPages.setPreferredSize(new Dimension(200, 500));
    scrollPages.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_ALWAYS);
    panel.add(scrollPages, constraints);
    constraints.gridy++;

    // Links count
    labelLinksCount = Utilities.createJLabel(GT._("Backlinks"));
    updateBacklinksCount();
    constraints.fill = GridBagConstraints.HORIZONTAL;
    constraints.weightx = 1;
    constraints.weighty = 0;
    panel.add(labelLinksCount, constraints);
    constraints.gridy++;

    // Toolbar
    JToolBar toolbar = new JToolBar(SwingConstants.HORIZONTAL);
    toolbar.setFloatable(false);
    buttonFullAnalysis = Utilities.createJButton(
        "gnome-system-run.png", EnumImageSize.NORMAL,
        GT._("Full analysis (Alt + &F)"), false);
    buttonFullAnalysis.setActionCommand(ACTION_FULL_ANALYSIS);
    buttonFullAnalysis.addActionListener(this);
    toolbar.add(buttonFullAnalysis);
    buttonDisambiguation = Utilities.createJButton(
        "commons-disambig-colour.png", EnumImageSize.NORMAL,
        GT._("Disambiguation (Alt + &D)"), false);
    buttonDisambiguation.setActionCommand(ACTION_DISAMBIGUATION);
    buttonDisambiguation.addActionListener(this);
    toolbar.add(buttonDisambiguation);
    buttonUpdateDabWarning = Utilities.createJButton(
        "gnome-dialog-warning.png", EnumImageSize.NORMAL,
        GT._("Update disambiguation warning"), false);
    buttonUpdateDabWarning.setActionCommand(ACTION_UPDATE_DAB);
    buttonUpdateDabWarning.addActionListener(this);
    toolbar.add(buttonUpdateDabWarning);
    buttonUpdateInfo = Utilities.createJButton(
        "gnome-view-refresh.png", EnumImageSize.NORMAL,
        GT._("Update page information (Alt + &U)"), false);
    buttonUpdateInfo.setActionCommand(ACTION_UPDATE);
    buttonUpdateInfo.addActionListener(this);
    toolbar.add(buttonUpdateInfo);
    buttonComments = Utilities.createJButton(
        "tango-internet-group-chat.png", EnumImageSize.NORMAL,
        GT._("Set page comments (Alt + &C)"), false);
    buttonComments.setActionCommand(ACTION_SET_COMMENTS);
    buttonComments.addActionListener(this);
    toolbar.add(buttonComments);
    if (watchList) {
      buttonRemove = Utilities.createJButton(
          "gnome-list-remove.png", EnumImageSize.NORMAL,
          GT._("Remove page (Alt + &R)"), false);
      buttonRemove.setActionCommand(ACTION_REMOVE);
      buttonRemove.addActionListener(this);
      toolbar.add(buttonRemove);
      buttonAdd = Utilities.createJButton(
          "gnome-list-add.png", EnumImageSize.NORMAL,
          GT._("Add page (Alt + &A)"), false);
      buttonAdd.setActionCommand(ACTION_ADD);
      buttonAdd.addActionListener(this);
      toolbar.add(buttonAdd);
    }
    constraints.fill = GridBagConstraints.HORIZONTAL;
    constraints.weightx = 1;
    constraints.weighty = 0;
    panel.add(toolbar, constraints);
    constraints.gridy++;

    return panel;
  }

  /**
   * Invoked when an action occurs.
   * 
   * @param e Event.
   */
  @Override
  public void actionPerformed(ActionEvent e) {
    if (e == null) {
      return;
    }

    if (ACTION_FULL_ANALYSIS.equals(e.getActionCommand())) {
      actionFullAnalysis();
    } else if (ACTION_DISAMBIGUATION.equals(e.getActionCommand())) {
      actionDisambiguation();
    } else if (ACTION_UPDATE_DAB.equals(e.getActionCommand())) {
      actionUpdateDabWarning();
    } else if (ACTION_REMOVE.equals(e.getActionCommand())) {
      actionRemove();
    } else if (ACTION_SET_COMMENTS.equals(e.getActionCommand())) {
      actionSetComments();
    } else if (ACTION_UPDATE.equals(e.getActionCommand())) {
      actionUpdateInfo();
    } else if (ACTION_ADD.equals(e.getActionCommand())) {
      actionAdd();
    }
  }

  /**
   * Action called when Full analysis button is pressed.
   */
  private void actionFullAnalysis() {
    Controller.runFullAnalysis(
        getParentComponent(),
        getSelectedPages(), null,
        getWikipedia());
  }

  /**
   * Action called when Disambiguation button is pressed.
   */
  private void actionDisambiguation() {
    Controller.runDisambiguationAnalysis(
        getParentComponent(),
        getSelectedPages(),
        getWikipedia());
  }

  /**
   * Action called when Update Dab Warning button is pressed.
   */
  private void actionUpdateDabWarning() {
    List<Page> tmpPages = new ArrayList<Page>();
    for (int i = 0; i < getSelectedPages().length; i++) {
      tmpPages.add(getSelectedPages()[i]);
    }
    if (tmpPages.isEmpty()) {
      return;
    }
    String template = getWikipedia().getDisambiguationWarningTemplate();
    if ((template == null) || (template.trim().length() == 0)) {
      Utilities.displayWarning(
          getParentComponent(),
          GT._("You need to define the 'dab_warning_template' property in WikiCleaner configuration."));
    }
    int answer = Utilities.displayYesNoWarning(
        getParentComponent(),
        GT._("Do you want to update the disambiguation warning in talk page ?"));
    if (answer != JOptionPane.YES_OPTION) {
      return;
    }
    UpdateDabWarningWorker worker = new UpdateDabWarningWorker(getWikipedia(), this, tmpPages);
    worker.start();
  }

  /**
   * Action called when Disambiguation button is pressed.
   */
  private void actionRemove() {
    if (displayYesNoWarning(GT._(
        "You are about to remove the pages from your local Watch list.\n" +
        "Are you sure ?")) != JOptionPane.YES_OPTION) {
      return;
    }
    Page[] selectedPages = getSelectedPages();
    modelPages.removePages(selectedPages);
    List<Page> tmpPages = modelPages.getPages();
    List<String> watchedPages = new ArrayList<String>(tmpPages.size());
    for (Page p : tmpPages) {
      watchedPages.add(p.getTitle());
    }
    Configuration config = Configuration.getConfiguration();
    config.setStringList(Configuration.ARRAY_WATCH_PAGES, watchedPages);
  }

  /**
   * Action called when Add button is pressed. 
   */
  private void actionAdd() {
    String value = askForValue(
        GT._("Enter the page title you want to add to your local watch list"),
        "", null);
    if (value != null) {
      Configuration config = Configuration.getConfiguration();
      List<String> watchedPages = config.getStringList(Configuration.ARRAY_WATCH_PAGES);
      if (!watchedPages.contains(value)) {
        watchedPages.add(value);
        Collections.sort(watchedPages);
        config.setStringList(Configuration.ARRAY_WATCH_PAGES, watchedPages);
        modelPages.addPage(DataManager.getPage(getWikipedia(), value, null, null));
      }
    }
  }

  /**
   * Action called when Set comments button is pressed.
   */
  private void actionSetComments() {
    Page[] selectedPages = getSelectedPages();
    Controller.runPageComments(selectedPages, getWikipedia());
  }

  /**
   * Action called when Update information button is pressed. 
   */
  private void actionUpdateInfo() {
    Page[] tmpPages = getSelectedPages();
    if ((tmpPages == null) || (tmpPages.length == 0)) {
      return;
    }
    final UpdateInfoWorker updateWorker = new UpdateInfoWorker(getWikipedia(), this, tmpPages);
    updateWorker.setListener(new DefaultBasicWorkerListener() {

      /* (non-Javadoc)
       * @see org.wikipediacleaner.gui.swing.basic.DefaultBasicWorkerListener#afterFinished(org.wikipediacleaner.gui.swing.basic.BasicWorker, boolean)
       */
      @Override
      public void afterFinished(BasicWorker worker, boolean ok) {
        super.afterFinished(worker, ok);
        updateBacklinksCount();
      }
      
    });
    updateWorker.start();
  }

  /**
   * Update the total count of backlinks.
   */
  void updateBacklinksCount() {
    int backlinksMain = 0;
    int backlinks = 0;
    int maxMain = 0;
    int max = 0;
    int actualMain = 0;
    int actual = 0;
    for (Page page : pages) {
      if (page != null) {
        PageComment comment = page.getComment();
        Integer tmpLinks = page.getBacklinksCountInMainNamespace();
        if (tmpLinks != null) {
          backlinksMain += tmpLinks.intValue();
          if ((comment != null) && (comment.getMaxMainArticles() != null)) {
            maxMain += comment.getMaxMainArticles().intValue();
            actualMain += tmpLinks.intValue();
          }
        }
        tmpLinks = page.getBacklinksCount();
        if (tmpLinks != null) {
          backlinks += tmpLinks.intValue();
          if ((comment != null) && (comment.getMaxArticles() != null)) {
            max += comment.getMaxArticles().intValue();
            actual += tmpLinks.intValue();
          }
        }
      }
    }
    String txtMain = null;
    if ((actualMain > 0) || (maxMain > 0)) {
      txtMain = "" + backlinksMain + " (" + actualMain + "/" + maxMain + ")";
    } else {
      txtMain = "" + backlinksMain;
    }
    String txtAll  = null;
    if ((actual > 0) || (max > 0)) {
      txtAll = "" + backlinks + " (" + actual + "/" + max + ")";
    } else {
      txtAll = "" + backlinks;
    }
    labelLinksCount.setText(GT._(
        "Backlinks - Main namespace: {0}, All namespaces: {1}",
        new Object[] { txtMain, txtAll }));
  }
  
  /**
   * @return Selected pages.
   */
  private Page[] getSelectedPages() {
    int[] rows = tablePages.getSelectedRows();
    for (int i = 0; i < rows.length; i++) {
      rows[i] = Utilities.convertRowIndexToModel(tablePages, rows[i]);
    }
    Page[] result = modelPages.getPages(rows);
    return result;
  }
}

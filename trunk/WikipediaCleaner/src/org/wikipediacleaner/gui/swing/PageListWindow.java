/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.gui.swing;

import java.awt.Component;
import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionListener;
import java.beans.EventHandler;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;

import javax.swing.JButton;
import javax.swing.JFileChooser;
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
import org.wikipediacleaner.api.constants.WPCConfigurationString;
import org.wikipediacleaner.api.data.DataManager;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.data.PageComment;
import org.wikipediacleaner.api.data.ProgressionValue;
import org.wikipediacleaner.gui.swing.basic.BasicWindow;
import org.wikipediacleaner.gui.swing.basic.BasicWorker;
import org.wikipediacleaner.gui.swing.basic.DefaultBasicWindowListener;
import org.wikipediacleaner.gui.swing.basic.DefaultBasicWorkerListener;
import org.wikipediacleaner.gui.swing.basic.Utilities;
import org.wikipediacleaner.gui.swing.component.IconCellRenderer;
import org.wikipediacleaner.gui.swing.component.ProgressionValueCellRenderer;
import org.wikipediacleaner.gui.swing.component.BooleanIconCellRenderer;
import org.wikipediacleaner.gui.swing.worker.PageListWorker;
import org.wikipediacleaner.gui.swing.worker.UpdateDabWarningWorker;
import org.wikipediacleaner.gui.swing.worker.UpdateInfoWorker;
import org.wikipediacleaner.i18n.GT;
import org.wikipediacleaner.images.EnumImageSize;
import org.wikipediacleaner.utils.Configuration;
import org.wikipediacleaner.utils.ConfigurationValueShortcut;


/**
 * Window displaying a List of pages.
 */
public class PageListWindow extends BasicWindow {

  public final static Integer WINDOW_VERSION = Integer.valueOf(2);

  String title;
  Page referencePage;
  List<Page> pages;
  boolean watchList;

  PageListTableModel modelPages;
  JTable tablePages;

  JLabel  labelLinksCount;
  
  private JButton buttonAdd;
  private JButton buttonAutomaticFixing;
  private JButton buttonComments;
  private JButton buttonDisambiguation;
  private JButton buttonDisambiguationWatch;
  private JButton buttonFullAnalysis;
  private JButton buttonRemove;
  private JButton buttonSave;
  private JButton buttonSelectDab;
  private JButton buttonUpdateDabWarning;
  private JButton buttonUpdateInfo;
  private JButton buttonView;
  private JButton buttonViewHistory;

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
      final Page referencePage,
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
              pageList.referencePage = referencePage;
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
    modelPages = new PageListTableModel(getWikipedia(), pages);
    tablePages = new JTable(modelPages);
    tablePages.setDefaultRenderer(ProgressionValue.class, new ProgressionValueCellRenderer());
    TableColumnModel columnModel = tablePages.getColumnModel();
    columnModel.getColumn(PageListTableModel.COLUMN_BACKLINKS_MAIN).setMinWidth(50);
    columnModel.getColumn(PageListTableModel.COLUMN_BACKLINKS_MAIN).setPreferredWidth(50);
    columnModel.getColumn(PageListTableModel.COLUMN_BACKLINKS_MAIN).setMaxWidth(100);
    columnModel.getColumn(PageListTableModel.COLUMN_BACKLINKS_OTHER).setMinWidth(50);
    columnModel.getColumn(PageListTableModel.COLUMN_BACKLINKS_OTHER).setPreferredWidth(50);
    columnModel.getColumn(PageListTableModel.COLUMN_BACKLINKS_OTHER).setMaxWidth(100);
    columnModel.getColumn(PageListTableModel.COLUMN_BACKLINKS_TEMPLATE).setMinWidth(40);
    columnModel.getColumn(PageListTableModel.COLUMN_BACKLINKS_TEMPLATE).setPreferredWidth(40);
    columnModel.getColumn(PageListTableModel.COLUMN_BACKLINKS_TEMPLATE).setMaxWidth(100);
    columnModel.getColumn(PageListTableModel.COLUMN_COMMENTS_TEXT).setMinWidth(60);
    columnModel.getColumn(PageListTableModel.COLUMN_DISAMBIGUATION).setMinWidth(20);
    columnModel.getColumn(PageListTableModel.COLUMN_DISAMBIGUATION).setPreferredWidth(20);
    columnModel.getColumn(PageListTableModel.COLUMN_DISAMBIGUATION).setMaxWidth(20);
    columnModel.getColumn(PageListTableModel.COLUMN_DISAMBIGUATION).setCellRenderer(
        new BooleanIconCellRenderer("commons-disambig-colour.png", null));
    columnModel.getColumn(PageListTableModel.COLUMN_DISAMBIGUATION).setHeaderRenderer(
        new IconCellRenderer("commons-disambig-colour.png"));
    columnModel.getColumn(PageListTableModel.COLUMN_REDIRECT).setMinWidth(20);
    columnModel.getColumn(PageListTableModel.COLUMN_REDIRECT).setPreferredWidth(20);
    columnModel.getColumn(PageListTableModel.COLUMN_REDIRECT).setMaxWidth(20);
    columnModel.getColumn(PageListTableModel.COLUMN_REDIRECT).setCellRenderer(
        new BooleanIconCellRenderer("commons-redirect-arrow-without-text.png", null));
    columnModel.getColumn(PageListTableModel.COLUMN_REDIRECT).setHeaderRenderer(
        new IconCellRenderer("commons-redirect-arrow-without-text.png"));
    columnModel.getColumn(PageListTableModel.COLUMN_PAGE).setMinWidth(100);
    columnModel.getColumn(PageListTableModel.COLUMN_PAGE).setPreferredWidth(200);
    columnModel.getColumn(PageListTableModel.COLUMN_WATCHED).setMinWidth(20);
    columnModel.getColumn(PageListTableModel.COLUMN_WATCHED).setPreferredWidth(20);
    columnModel.getColumn(PageListTableModel.COLUMN_WATCHED).setMaxWidth(20);
    columnModel.getColumn(PageListTableModel.COLUMN_WATCHED).setCellRenderer(
        new BooleanIconCellRenderer("gnome-logviewer.png", null));
    columnModel.getColumn(PageListTableModel.COLUMN_WATCHED).setHeaderRenderer(
        new IconCellRenderer("gnome-logviewer.png"));
    Utilities.addRowSorter(tablePages, modelPages);
    tablePages.addMouseListener(new PageListMouseListener());
    JScrollPane scrollPages = new JScrollPane(tablePages);
    scrollPages.setMinimumSize(new Dimension(300, 200));
    scrollPages.setPreferredSize(new Dimension(450, 500));
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

    // Tool bar
    JToolBar toolbar = new JToolBar(SwingConstants.HORIZONTAL);
    toolbar.setFloatable(false);
    toolbar.setBorderPainted(false);
    buttonFullAnalysis = Utilities.createJButton(
        "gnome-system-run.png", EnumImageSize.NORMAL,
        GT._("Full analysis"), false,
        ConfigurationValueShortcut.FULL_ANALYSIS);
    buttonFullAnalysis.addActionListener(EventHandler.create(
        ActionListener.class, this, "actionFullAnalysis"));
    toolbar.add(buttonFullAnalysis);

    buttonDisambiguation = Utilities.createJButton(
        "commons-disambig-colour.png", EnumImageSize.NORMAL,
        GT._("Disambiguation"), false,
        ConfigurationValueShortcut.DAB_ANALYSIS);
    buttonDisambiguation.addActionListener(EventHandler.create(
        ActionListener.class, this, "actionDisambiguation"));
    toolbar.add(buttonDisambiguation);

    buttonSelectDab = Utilities.createJButton(
        "wpc-select-disambig.png", EnumImageSize.NORMAL,
        GT._("Select disambiguation pages with too many backlinks"),
        false, null);
    buttonSelectDab.addActionListener(EventHandler.create(
        ActionListener.class, this, "actionSelectDisambiguation"));
    toolbar.add(buttonSelectDab);

    buttonDisambiguationWatch = Utilities.createJButton(
        "commons-disambig-colour-plus.png", EnumImageSize.NORMAL,
        GT._("Analyze pages with links to disambiguation pages"),
        false, null);
    buttonDisambiguationWatch.addActionListener(EventHandler.create(
        ActionListener.class, this, "actionDisambiguationWatch"));
    toolbar.add(buttonDisambiguationWatch);

    buttonUpdateDabWarning = Utilities.createJButton(
        "gnome-dialog-warning.png", EnumImageSize.NORMAL,
        GT._("Update disambiguation warning"),
        false, null);
    buttonUpdateDabWarning.addActionListener(EventHandler.create(
        ActionListener.class, this, "actionUpdateDabWarning"));
    toolbar.add(buttonUpdateDabWarning);

    buttonUpdateInfo = Utilities.createJButton(
        "gnome-view-refresh.png", EnumImageSize.NORMAL,
        GT._("Update page information (Alt + &U)"), false, null);
    buttonUpdateInfo.addActionListener(EventHandler.create(
        ActionListener.class, this, "actionUpdateInfo"));
    toolbar.add(buttonUpdateInfo);

    buttonComments = Utilities.createJButton(
        "tango-internet-group-chat.png", EnumImageSize.NORMAL,
        GT._("Set page comments (Alt + &C)"), false, null);
    buttonComments.addActionListener(EventHandler.create(
        ActionListener.class, this, "actionSetComments"));
    toolbar.add(buttonComments);

    buttonView = Utilities.createJButton(
        "gnome-emblem-web.png", EnumImageSize.NORMAL,
        GT._("External Viewer"), false,
        ConfigurationValueShortcut.EXTERNAL_VIEWER);
    buttonView.addActionListener(EventHandler.create(
        ActionListener.class, this, "actionView"));
    toolbar.add(buttonView);

    buttonViewHistory = Utilities.createJButton(
        "gnome-emblem-documents.png", EnumImageSize.NORMAL,
        GT._("History"), false,
        ConfigurationValueShortcut.HISTORY);
    buttonViewHistory.addActionListener(EventHandler.create(
        ActionListener.class, this, "actionViewHistory"));
    toolbar.add(buttonViewHistory);

    buttonSave = Utilities.createJButton(
        "gnome-document-save.png", EnumImageSize.NORMAL,
        GT._("Save list"), false, null);
    buttonSave.addActionListener(EventHandler.create(
        ActionListener.class, this, "actionSave"));
    toolbar.add(buttonSave);

    buttonAutomaticFixing = Utilities.createJButton(GT._("Automatic fixing"), null);
    buttonAutomaticFixing.addActionListener(EventHandler.create(
        ActionListener.class, this, "actionRunAutomaticFixing"));
    toolbar.add(buttonAutomaticFixing);

    if (watchList) {
      buttonRemove = Utilities.createJButton(
          "gnome-list-remove.png", EnumImageSize.NORMAL,
          GT._("Remove page"), false,
          ConfigurationValueShortcut.LIST_REMOVE);
      buttonRemove.addActionListener(EventHandler.create(
          ActionListener.class, this, "actionRemove"));
      toolbar.add(buttonRemove);
      buttonAdd = Utilities.createJButton(
          "gnome-list-add.png", EnumImageSize.NORMAL,
          GT._("Add page"), false,
          ConfigurationValueShortcut.LIST_ADD);
      buttonAdd.addActionListener(EventHandler.create(
          ActionListener.class, this, "actionAdd"));
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
   * Action called when Full analysis button is pressed.
   */
  public void actionFullAnalysis() {
    Controller.runFullAnalysis(
        getParentComponent(),
        getSelectedPages(), null,
        getWikipedia());
  }

  /**
   * Action called when Disambiguation button is pressed.
   */
  public void actionDisambiguation() {
    Controller.runDisambiguationAnalysis(
        getParentComponent(),
        getSelectedPages(),
        getWikipedia());
  }

  /**
   * Action called when Select Disambiguation button is pressed.
   */
  public void actionSelectDisambiguation() {
    tablePages.clearSelection();
    int index = 0;
    for (Page page : pages) {
      if (page.isDisambiguationPage()) {
        if ((page.getComment() != null) &&
            (page.getComment().getMaxMainArticles() != null)) {
          int maxArticles = page.getComment().getMaxMainArticles().intValue();
          Integer articles = page.getBacklinksCountInMainNamespace();
          if ((articles != null) && (maxArticles < articles.intValue())) {
            int tmpIndex = tablePages.convertRowIndexToView(index);
            tablePages.addRowSelectionInterval(tmpIndex, tmpIndex);
          }
        }
      }
      index++;
    }
  }

  /**
   * Action called when Disambiguation Watch button is pressed.
   */
  public void actionDisambiguationWatch() {
    Page[] selectedPages = getSelectedPages();
    if ((selectedPages == null) || (selectedPages.length == 0)) {
      return;
    }
    List<String> pageNames = new ArrayList<String>(selectedPages.length);
    for (Page page : selectedPages) {
      pageNames.add(page.getTitle());
    }
    new PageListWorker(
        getWikipedia(), this, null,
        pageNames,
        PageListWorker.Mode.DAB_WATCH, false,
        GT._("Articles with links to disambiguation pages")).start();
  }

  /**
   * Action called when Update Dab Warning button is pressed.
   */
  public void actionUpdateDabWarning() {
    List<Page> tmpPages = new ArrayList<Page>();
    for (int i = 0; i < getSelectedPages().length; i++) {
      tmpPages.add(getSelectedPages()[i]);
    }
    if (tmpPages.isEmpty()) {
      return;
    }
    String template = getConfiguration().getString(WPCConfigurationString.DAB_WARNING_TEMPLATE);
    if ((template == null) || (template.trim().length() == 0)) {
      Utilities.displayMessageForMissingConfiguration(
          getParentComponent(),
          WPCConfigurationString.DAB_WARNING_TEMPLATE.getAttributeName());
    }
    int answer = Utilities.displayYesNoWarning(
        getParentComponent(),
        GT._("Do you want to update the disambiguation warning on talk page?"));
    if (answer != JOptionPane.YES_OPTION) {
      return;
    }
    UpdateDabWarningWorker worker = new UpdateDabWarningWorker(getWikipedia(), this, tmpPages, false);
    worker.start();
  }

  /**
   * Action called when Disambiguation button is pressed.
   */
  public void actionRemove() {
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
    config.setStringList(getWikipedia(), Configuration.ARRAY_WATCH_PAGES, watchedPages);
  }

  /**
   * Action called when Add button is pressed. 
   */
  public void actionAdd() {
    String value = askForValue(
        GT._("Enter the page title you want to add to your local watch list"),
        "", null);
    if (value != null) {
      Configuration config = Configuration.getConfiguration();
      List<String> watchedPages = config.getStringList(getWikipedia(), Configuration.ARRAY_WATCH_PAGES);
      if (!watchedPages.contains(value)) {
        watchedPages.add(value);
        Collections.sort(watchedPages);
        config.setStringList(getWikipedia(), Configuration.ARRAY_WATCH_PAGES, watchedPages);
        modelPages.addPage(DataManager.getPage(getWikipedia(), value, null, null, null));
      }
    }
  }

  /**
   * Action called when Set comments button is pressed.
   */
  public void actionSetComments() {
    Page[] selectedPages = getSelectedPages();
    Controller.runPageComments(selectedPages, getWikipedia());
  }

  /**
   * Action called when Update information button is pressed. 
   */
  public void actionUpdateInfo() {
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
        modelPages.updateWatchedPages();
        super.afterFinished(worker, ok);
        updateBacklinksCount();
      }
      
    });
    updateWorker.start();
  }

  /**
   * Action called when View button is pressed. 
   */
  public void actionView() {
    Page[] tmpPages = getSelectedPages();
    if (tmpPages != null) {
      for (Page page : tmpPages) {
        Utilities.browseURL(getWikipedia(), page.getTitle(), false);
      }
    }
  }

  /**
   * Action called when View History button is pressed. 
   */
  public void actionViewHistory() {
    Page[] tmpPages = getSelectedPages();
    if (tmpPages != null) {
      for (Page page : tmpPages) {
        Utilities.browseURL(getWikipedia(), page.getTitle(), "history");
      }
    }
  }

  /**
   * Action called when Save button is pressed.
   */
  public void actionSave() {
    JFileChooser fileChooser = new JFileChooser();
    fileChooser.setCurrentDirectory(new File("."));
    fileChooser.setDialogTitle(GT._("Save list"));
    int answer = fileChooser.showSaveDialog(this.getParentComponent());
    if (answer != JFileChooser.APPROVE_OPTION) {
      return;
    }
    File file = fileChooser.getSelectedFile();
    if (file == null) {
      return;
    }
    BufferedWriter buffer = null;
    try {
      buffer = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(file), "UTF8"));
      for (Page page : pages) {
        buffer.append("* [[:" + page.getTitle() + "]]\n");
      }
    } catch (IOException e) {
      // Nothing to do
    } finally {
      if (buffer != null) {
        try {
          buffer.close();
        } catch (IOException e) {
          // Nothing to do
        }
      }
    }
  }

  /**
   * Action called when Run Automatic Fixing button is pressed. 
   */
  public void actionRunAutomaticFixing() {
    Page[] values = getSelectedPages();
    if ((values == null) || (values.length == 0)) {
      Utilities.displayWarning(
          getParentComponent(),
          GT._("You must select pages on which running automatic fixing."));
      return;
    }
    Collection<Page> tmpPages = new ArrayList<Page>(values.length);
    for (int i = 0; i < values.length; i++) {
      tmpPages.add(values[i]);
    }
    Controller.runAutomatixFixing(pages, referencePage, getWikipedia());
  }

  /**
   * Update the total count of backlinks.
   */
  void updateBacklinksCount() {
    int backlinksMain = 0;
    int backlinks = 0;
    int maxMain = 0;
    int actualMain = 0;
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
        }
      }
    }
    String txtMain = null;
    if ((actualMain > 0) || (maxMain > 0)) {
      if (backlinksMain != actualMain) {
        if (actualMain != maxMain) {
          txtMain = "" + backlinksMain + " (" + actualMain + "/" + maxMain + ")";
        } else {
          txtMain = "" + backlinksMain + " (" + actualMain + ")";
        }
      } else if (actualMain != maxMain) {
        txtMain = "" + actualMain + "/" + maxMain;
      } else {
        txtMain = "" + actualMain;
      }
    } else {
      txtMain = "" + backlinksMain;
    }
    String txtAll  = null;
    txtAll = "" + backlinks;
    labelLinksCount.setText(
        GT.__("{0} page", "{0} pages", pages.size(), Integer.toString(pages.size())) +
        ", " +
        GT._(
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

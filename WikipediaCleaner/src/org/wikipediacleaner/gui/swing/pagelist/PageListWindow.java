/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.gui.swing.pagelist;

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
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Optional;

import javax.swing.JButton;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JToolBar;
import javax.swing.ScrollPaneConstants;
import javax.swing.SwingConstants;
import javax.swing.WindowConstants;

import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.DataManager;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.data.page.PageComment;
import org.wikipediacleaner.gui.swing.Controller;
import org.wikipediacleaner.gui.swing.action.ActionCheckArticle;
import org.wikipediacleaner.gui.swing.action.ActionUpdateWarning;
import org.wikipediacleaner.gui.swing.basic.BasicWindow;
import org.wikipediacleaner.gui.swing.basic.BasicWorker;
import org.wikipediacleaner.gui.swing.basic.DefaultBasicWindowListener;
import org.wikipediacleaner.gui.swing.basic.DefaultBasicWorkerListener;
import org.wikipediacleaner.gui.swing.basic.Utilities;
import org.wikipediacleaner.gui.swing.deadlink.ActionDeadLink;
import org.wikipediacleaner.gui.swing.linter.ActionLinter;
import org.wikipediacleaner.gui.swing.worker.CheckArticleWorker;
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
  Map<String, PageComment> commentsByPageTitle;
  boolean watchList;

  PageListTable tablePages;

  JLabel  labelLinksCount;
  
  private JButton buttonAdd;
  private JButton buttonAutomaticFixing;
  private JButton buttonCheckArticle;
  private JButton buttonComments;
  private JButton buttonDisambiguation;
  private JButton buttonDisambiguationWatch;
  private JButton buttonFullAnalysis;
  private JButton buttonRemove;
  private JButton buttonSave;
  private JButton buttonSelectDab;
  private JButton buttonUpdateInfo;
  private JButton buttonView;
  private JButton buttonViewHistory;

  /**
   * Create and display a PageListWindow.
   * 
   * @param title Window title.
   * @param referencePage Reference page.
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
              pageList.commentsByPageTitle = PageComment.get(wikipedia, pages);
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
    tablePages = PageListTable.createTable(getWikipedia(), pages, commentsByPageTitle);
    JScrollPane scrollPages = new JScrollPane(tablePages);
    scrollPages.setMinimumSize(new Dimension(300, 200));
    scrollPages.setPreferredSize(new Dimension(450, 500));
    scrollPages.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_ALWAYS);
    panel.add(scrollPages, constraints);
    constraints.gridy++;

    // Links count
    labelLinksCount = Utilities.createJLabel(GT._T("Backlinks"));
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
        GT._T("Full analysis"), false,
        ConfigurationValueShortcut.FULL_ANALYSIS);
    buttonFullAnalysis.addActionListener(EventHandler.create(
        ActionListener.class, this, "actionFullAnalysis"));
    toolbar.add(buttonFullAnalysis);

    buttonDisambiguation = Utilities.createJButton(
        "commons-disambig-colour.png", EnumImageSize.NORMAL,
        GT._T("Disambiguation"), false,
        ConfigurationValueShortcut.DAB_ANALYSIS);
    buttonDisambiguation.addActionListener(EventHandler.create(
        ActionListener.class, this, "actionDisambiguation"));
    toolbar.add(buttonDisambiguation);

    buttonCheckArticle = ActionCheckArticle.createInternalButton(
        true, false, true);
    buttonCheckArticle.addActionListener(EventHandler.create(
        ActionListener.class, this, "actionCheckArticle"));
    toolbar.add(buttonCheckArticle);

    buttonSelectDab = Utilities.createJButton(
        "wpc-select-disambig.png", EnumImageSize.NORMAL,
        GT._T("Select disambiguation pages with too many backlinks"),
        false, null);
    buttonSelectDab.addActionListener(EventHandler.create(
        ActionListener.class, this, "actionSelectDisambiguation"));
    toolbar.add(buttonSelectDab);

    buttonDisambiguationWatch = Utilities.createJButton(
        "commons-disambig-colour-plus.png", EnumImageSize.NORMAL,
        GT._T("Analyze pages with links to disambiguation pages"),
        false, null);
    buttonDisambiguationWatch.addActionListener(EventHandler.create(
        ActionListener.class, this, "actionDisambiguationWatch"));
    toolbar.add(buttonDisambiguationWatch);

    ActionUpdateWarning.addButton(
        getParentComponent(), this, toolbar, tablePages, true, false);

    buttonUpdateInfo = Utilities.createJButton(
        "gnome-view-refresh.png", EnumImageSize.NORMAL,
        GT._T("Update page information (Alt + &U)"), false, null);
    buttonUpdateInfo.addActionListener(EventHandler.create(
        ActionListener.class, this, "actionUpdateInfo"));
    toolbar.add(buttonUpdateInfo);

    buttonComments = Utilities.createJButton(
        "tango-internet-group-chat.png", EnumImageSize.NORMAL,
        GT._T("Set page comments (Alt + &C)"), false, null);
    buttonComments.addActionListener(EventHandler.create(
        ActionListener.class, this, "actionSetComments"));
    toolbar.add(buttonComments);

    buttonView = Utilities.createJButton(
        "gnome-emblem-web.png", EnumImageSize.NORMAL,
        GT._T("External Viewer"), false,
        ConfigurationValueShortcut.EXTERNAL_VIEWER);
    buttonView.addActionListener(EventHandler.create(
        ActionListener.class, this, "actionView"));
    toolbar.add(buttonView);

    buttonViewHistory = Utilities.createJButton(
        "gnome-emblem-documents.png", EnumImageSize.NORMAL,
        GT._T("History"), false,
        ConfigurationValueShortcut.HISTORY);
    buttonViewHistory.addActionListener(EventHandler.create(
        ActionListener.class, this, "actionViewHistory"));
    toolbar.add(buttonViewHistory);

    buttonSave = Utilities.createJButton(
        "gnome-document-save.png", EnumImageSize.NORMAL,
        GT._T("Save list"), false, null);
    buttonSave.addActionListener(EventHandler.create(
        ActionListener.class, this, "actionSave"));
    toolbar.add(buttonSave);

    buttonAutomaticFixing = Utilities.createJButton(
        "gnome-view-sort-descending.png", EnumImageSize.NORMAL,
        GT._T("Automatic fixing"), false, null);
    buttonAutomaticFixing.addActionListener(EventHandler.create(
        ActionListener.class, this, "actionRunAutomaticFixing"));
    toolbar.add(buttonAutomaticFixing);

    ActionLinter.addButton(
        this, toolbar,
        getWiki(), new PageListTableSelectedPages(tablePages),
        true);
    ActionDeadLink.addButton(
        this, toolbar,
        getWiki(), new PageListTableSelectedPages(tablePages),
        true);

    if (watchList) {
      buttonRemove = Utilities.createJButton(
          "gnome-list-remove.png", EnumImageSize.NORMAL,
          GT._T("Remove page"), false,
          ConfigurationValueShortcut.LIST_REMOVE);
      buttonRemove.addActionListener(EventHandler.create(
          ActionListener.class, this, "actionRemove"));
      toolbar.add(buttonRemove);
      buttonAdd = Utilities.createJButton(
          "gnome-list-add.png", EnumImageSize.NORMAL,
          GT._T("Add page"), false,
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
    List<Page> selectedPages = tablePages.getSelectedPages();
    Controller.runFullAnalysis(
        getParentComponent(),
        selectedPages, null,
        getWikipedia());
  }

  /**
   * Action called when Disambiguation button is pressed.
   */
  public void actionDisambiguation() {
    List<Page> selectedPages = tablePages.getSelectedPages();
    Controller.runDisambiguationAnalysis(
        getParentComponent(),
        selectedPages,
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
        Integer maxMainArticles = Optional
            .ofNullable(commentsByPageTitle.get(page.getTitle()))
            .map(PageComment::getMaxMainArticles)
            .orElse(null);
        if (maxMainArticles != null) {
          int maxArticles = maxMainArticles.intValue();
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
    List<Page> selectedPages = tablePages.getSelectedPages();
    if ((selectedPages == null) ||
        (selectedPages.size() == 0)) {
      return;
    }
    List<String> pageNames = new ArrayList<>(selectedPages.size());
    for (Page page : selectedPages) {
      pageNames.add(page.getTitle());
    }
    new PageListWorker(
        getWikipedia(), this, null,
        pageNames,
        PageListWorker.Mode.DAB_WATCH, false,
        GT._T("Articles with links to disambiguation pages")).start();
  }

  /**
   * Action called when Remove button is pressed.
   */
  public void actionRemove() {
    if (displayYesNoWarning(GT._T(
        "You are about to remove the pages from your local watchlist.\n" +
        "Are you sure?")) != JOptionPane.YES_OPTION) {
      return;
    }
    List<Page> selectedPages = tablePages.getSelectedPages();
    if ((selectedPages == null) || (selectedPages.isEmpty())) {
      return;
    }
    Configuration config = Configuration.getConfiguration();
    List<String> watchedPages = config.getStringList(
        getWikipedia(), Configuration.ARRAY_WATCH_PAGES);
    for (Page page : selectedPages) {
      watchedPages.remove(page.getTitle());
    }
    config.setStringList(
        getWikipedia(), Configuration.ARRAY_WATCH_PAGES, watchedPages);
    tablePages.removePages(selectedPages);
  }

  /**
   * Action called when Add button is pressed. 
   */
  public void actionAdd() {
    String value = askForValue(
        GT._T("Enter the page title you want to add to your local watchlist"),
        "", null);
    if (value != null) {
      Page page = DataManager.getPage(getWikipedia(), value, null, null, null);
      Configuration config = Configuration.getConfiguration();
      List<String> watchedPages = config.getStringList(
          getWikipedia(), Configuration.ARRAY_WATCH_PAGES);
      if (!watchedPages.contains(page.getTitle())) {
        watchedPages.add(page.getTitle());
        Collections.sort(watchedPages);
        config.setStringList(getWikipedia(), Configuration.ARRAY_WATCH_PAGES, watchedPages);
        tablePages.addPage(page);
      }
    }
  }

  /**
   * Action called when Set comments button is pressed.
   */
  public void actionSetComments() {
    List<Page> selectedPages = tablePages.getSelectedPages();
    for (Page selectedPage : selectedPages) {
      commentsByPageTitle.computeIfAbsent(
          selectedPage.getTitle(),
          pageTitle -> PageComment.getOrCreate(getWiki(), pageTitle));
    }
    Controller.runPageComments(selectedPages, commentsByPageTitle, getWikipedia());
  }

  /**
   * Action called when Check article button is pressed. 
   */
  public void actionCheckArticle() {
    List<Page> selectedPages = tablePages.getSelectedPages();
    if ((selectedPages == null) || (selectedPages.size() == 0)) {
      return;
    }
    final CheckArticleWorker checkWorker = new CheckArticleWorker(
        getWikipedia(), this, selectedPages);
    checkWorker.start();
  }

  /**
   * Action called when Update information button is pressed. 
   */
  public void actionUpdateInfo() {
    List<Page> selectedPages = tablePages.getSelectedPages();
    if ((selectedPages == null) || (selectedPages.size() == 0)) {
      return;
    }
    final UpdateInfoWorker updateWorker = new UpdateInfoWorker(
        getWikipedia(), this, selectedPages);
    updateWorker.setListener(new DefaultBasicWorkerListener() {

      /* (non-Javadoc)
       * @see org.wikipediacleaner.gui.swing.basic.DefaultBasicWorkerListener#afterFinished(org.wikipediacleaner.gui.swing.basic.BasicWorker, boolean)
       */
      @Override
      public void afterFinished(BasicWorker worker, boolean ok) {
        tablePages.updateWatchedPages();
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
    List<Page> selectedPages = tablePages.getSelectedPages();
    if (selectedPages != null) {
      for (Page page : selectedPages) {
        Utilities.browseURL(getWikipedia(), page.getTitle(), false);
      }
    }
  }

  /**
   * Action called when View History button is pressed. 
   */
  public void actionViewHistory() {
    List<Page> selectedPages = tablePages.getSelectedPages();
    if (selectedPages != null) {
      for (Page page : selectedPages) {
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
    fileChooser.setDialogTitle(GT._T("Save list"));
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
    List<Page> selectedPages = tablePages.getSelectedPages();
    if ((selectedPages == null) || (selectedPages.size() == 0)) {
      Utilities.displayWarning(
          getParentComponent(),
          GT._T("You must select pages on which running automatic fixing."));
      return;
    }
    Controller.runAutomaticFixing(selectedPages, referencePage, getWikipedia());
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
        Integer tmpLinks = page.getBacklinksCountInMainNamespace();
        if (tmpLinks != null) {
          backlinksMain += tmpLinks.intValue();
          Integer maxMainArticles = Optional
              .ofNullable(commentsByPageTitle.get(page.getTitle()))
              .map(PageComment::getMaxMainArticles)
              .orElse(null);
          if (maxMainArticles != null) {
            maxMain += maxMainArticles.intValue();
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
        GT._T(
            "Backlinks - Main namespace: {0}, All namespaces: {1}",
            new Object[] { txtMain, txtAll }));
  }

}

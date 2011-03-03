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
import java.awt.FlowLayout;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Properties;

import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.DefaultListModel;
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JSplitPane;
import javax.swing.JToolBar;
import javax.swing.ListSelectionModel;
import javax.swing.ScrollPaneConstants;
import javax.swing.SwingConstants;
import javax.swing.WindowConstants;

import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.AutomaticFixing;
import org.wikipediacleaner.api.data.CompositeComparator;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.data.PageComparator;
import org.wikipediacleaner.gui.swing.action.SetComparatorAction;
import org.wikipediacleaner.gui.swing.basic.BasicWindow;
import org.wikipediacleaner.gui.swing.basic.BasicWorker;
import org.wikipediacleaner.gui.swing.basic.DefaultBasicWindowListener;
import org.wikipediacleaner.gui.swing.basic.DefaultBasicWorkerListener;
import org.wikipediacleaner.gui.swing.basic.Utilities;
import org.wikipediacleaner.gui.swing.component.PageListAnalyzeListener;
import org.wikipediacleaner.gui.swing.component.PageListCellRenderer;
import org.wikipediacleaner.gui.swing.component.PageListModel;
import org.wikipediacleaner.gui.swing.component.PageListPopupListener;
import org.wikipediacleaner.gui.swing.worker.AutomaticDisambiguationWorker;
import org.wikipediacleaner.gui.swing.worker.DisambiguationAnalysisWorker;
import org.wikipediacleaner.i18n.GT;
import org.wikipediacleaner.images.EnumImageSize;
import org.wikipediacleaner.utils.Configuration;


/**
 * Disambiguation window.
 */
public class DisambiguationWindow extends PageWindow {

  private final static String ACTION_ADD_AUTOMATIC_FIXING  = "ADD AUTOMATIC FIXING";
  private final static String ACTION_CLR_AUTOMATIC_FIXING  = "CLR AUTOMATIC FIXING";
  private final static String ACTION_DISAMBIGUATION_LINK   = "DISAMBIGUATION LINK";
  private final static String ACTION_EXTERNAL_VIEWER_LINK  = "EXTERNAL VIEWER LINK";
  private final static String ACTION_FULL_ANALYSIS_LINK    = "FULL ANALYSIS LINK";
  private final static String ACTION_MDF_AUTOMATIC_FIXING  = "MDF AUTOMATIC FIXING";
  private final static String ACTION_NEXT_LINKS            = "NEXT LINKS";
  private final static String ACTION_RMV_AUTOMATIC_FIXING  = "RMV AUTOMATIC FIXING";
  private final static String ACTION_RUN_AUTOMATIC_FIXING  = "RUN AUTOMATIC FIXING";
  private final static String ACTION_SAVE_AUTOMATIC_FIXING = "SAVE AUTOMATIC FIXING";

  //public final static Integer WINDOW_VERSION = Integer.valueOf(2);

  private JList listAutomaticFixing;
  private DefaultListModel modelAutomaticFixing;
  private JButton buttonAddAutomaticFixing;
  private JButton buttonMdfAutomaticFixing;
  private JButton buttonRmvAutomaticFixing;
  private JButton buttonClrAutomaticFixing;
  private JButton buttonRunAutomaticFixing;
  private JButton buttonSaveAutomaticFixing;

  JList listLinks;
  PageListModel modelLinks;
  JLabel linkCount;
  private JButton buttonFullAnalysisLink;
  private JButton buttonDisambiguationLink;
  private JButton buttonExternalViewerLink;
  private JButton buttonSelectNextLinks;

  /**
   * Create and display a DisambiguationWindow.
   * 
   * @param page Page name.
   * @param wikipedia Wikipedia.
   */
  public static void createDisambiguationWindow(
      final String page,
      final EnumWikipedia wikipedia) {
    createWindow(
        "DisambiguationWindow",
        wikipedia,
        WindowConstants.DISPOSE_ON_CLOSE,
        DisambiguationWindow.class,
        new DefaultBasicWindowListener() {
          @Override
          public void initializeWindow(BasicWindow window) {
            if (window instanceof DisambiguationWindow) {
              DisambiguationWindow disambig = (DisambiguationWindow) window;
              disambig.setPageName(page);
              disambig.modelLinks = new PageListModel();
              disambig.modelLinks.setComparator(PageComparator.getTemplateFirstComparator());
              disambig.modelLinks.setShowDisambiguation(true);
              disambig.modelLinks.setShowOther(true);
            }
          }
          @Override
          public void displayWindow(BasicWindow window) {
            if (window instanceof DisambiguationWindow) {
              DisambiguationWindow disambig = (DisambiguationWindow) window;
              disambig.actionReload();
            }
          }
        });
  }

  /* (non-Javadoc)
   * @see org.wikipediacleaner.gui.swing.basic.BasicWindow#getTitle()
   */
  @Override
  public String getTitle() {
    return GT._("Disambiguation - {0}", getPageName());
  }

  /**
   * @return Menu bar.
   */
  @Override
  protected JMenuBar createMenuBar() {
    JMenuBar menuBar = new JMenuBar();
    menuBar.add(createToolsMenu());
    menuBar.add(createSortMenu());
    menuBar.add(Box.createHorizontalGlue());
    return menuBar;
  }

  /**
   * @return Window components.
   */
  @Override
  protected Component createComponents() {
    createElements();
    JPanel panel = new JPanel(new GridBagLayout());

    // Initialize constraints
    GridBagConstraints constraints = new GridBagConstraints();
    constraints.fill = GridBagConstraints.HORIZONTAL;
    constraints.gridheight = 1;
    constraints.gridwidth = 1;
    constraints.gridx = 0;
    constraints.gridy = 0;
    constraints.insets = new Insets(2, 2, 2, 2);
    constraints.ipadx = 0;
    constraints.ipady = 0;
    constraints.weightx = 0;
    constraints.weighty = 0;

    // Page name
    //constraints.gridwidth = 2;
    panel.add(createPageComponents(), constraints);
    constraints.gridy++;

    // Contents
    constraints.fill = GridBagConstraints.BOTH;
    constraints.gridwidth = 1;
    constraints.gridx = 0;
    constraints.weightx = 1;
    constraints.weighty = 1;
    JSplitPane split = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT);
    split.setLeftComponent(createLinksComponents());
    split.setRightComponent(createContentsComponents());
    split.setPreferredSize(new Dimension(1200, 700));
    split.setMinimumSize(new Dimension(200, 200));
    split.setResizeWeight(0.0);
    split.setDividerLocation(200 + split.getInsets().left);
    panel.add(split, constraints);
    constraints.gridy++;

    updateComponentState();
    return panel;
  }

  /**
   * Creates internal elements. 
   */
  private void createElements() {
    createTextContents(this);
  }

  /**
   * @return Page components.
   */
  private Component createPageComponents() {
    JPanel panel = new JPanel(new FlowLayout(FlowLayout.CENTER));
    addTextPageName(panel);
    JToolBar toolbar = new JToolBar(SwingConstants.HORIZONTAL);
    toolbar.setFloatable(false);
    addButtonReload(toolbar, true);
    addButtonView(toolbar, true);
    addButtonSend(toolbar, true);
    addButtonWatch(toolbar, true);
    addButtonFullAnalysis(toolbar, true);
    panel.add(toolbar);
    return panel;
  }

  /**
   * @return Contents components.
   */
  private Component createContentsComponents() {
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
    constraints.weightx = 1;
    constraints.weighty = 0;

    // Contents
    constraints.fill = GridBagConstraints.BOTH;
    constraints.weighty = 1;
    addTextContents(panel, constraints);
    constraints.gridy++;

    // Semi-Automatic changes
    constraints.weighty = 0;
    panel.add(createAutomaticFixingComponents(), constraints);
    constraints.gridy++;

    return panel;
  }

  /**
   * @return Automatic fixing components.
   */
  private Component createAutomaticFixingComponents() {
    JPanel panel = new JPanel(new GridBagLayout());
    panel.setBorder(BorderFactory.createTitledBorder(
        BorderFactory.createEtchedBorder(),
        GT._("Automatic disambiguation fixing")));

    // Initialize constraints
    GridBagConstraints constraints = new GridBagConstraints();
    constraints.fill = GridBagConstraints.VERTICAL;
    constraints.gridheight = 1;
    constraints.gridwidth = 1;
    constraints.gridx = 0;
    constraints.gridy = 0;
    constraints.insets = new Insets(1, 1, 1, 1);
    constraints.ipadx = 0;
    constraints.ipady = 0;
    constraints.weightx = 0;
    constraints.weighty = 1;

    // Commands
    JToolBar toolBarButtons = new JToolBar(SwingConstants.VERTICAL);
    toolBarButtons.setFloatable(false);
    buttonAddAutomaticFixing = Utilities.createJButton(
        "gnome-list-add.png", EnumImageSize.NORMAL,
        GT._("Add"), false);
    buttonAddAutomaticFixing.setActionCommand(ACTION_ADD_AUTOMATIC_FIXING);
    buttonAddAutomaticFixing.addActionListener(this);
    toolBarButtons.add(buttonAddAutomaticFixing);
    buttonRmvAutomaticFixing = Utilities.createJButton(
        "gnome-list-remove.png", EnumImageSize.NORMAL,
        GT._("Remove"), false);
    buttonRmvAutomaticFixing.setActionCommand(ACTION_RMV_AUTOMATIC_FIXING);
    buttonRmvAutomaticFixing.addActionListener(this);
    toolBarButtons.add(buttonRmvAutomaticFixing);
    buttonMdfAutomaticFixing = Utilities.createJButton(GT._("Modify"));
    buttonMdfAutomaticFixing.setActionCommand(ACTION_MDF_AUTOMATIC_FIXING);
    buttonMdfAutomaticFixing.addActionListener(this);
    toolBarButtons.add(buttonMdfAutomaticFixing);
    buttonClrAutomaticFixing = Utilities.createJButton(
        "gnome-edit-clear.png", EnumImageSize.NORMAL,
        GT._("Clear"), false);
    buttonClrAutomaticFixing.setActionCommand(ACTION_CLR_AUTOMATIC_FIXING);
    buttonClrAutomaticFixing.addActionListener(this);
    toolBarButtons.add(buttonClrAutomaticFixing);
    buttonSaveAutomaticFixing = Utilities.createJButton(
        "gnome-media-floppy.png", EnumImageSize.NORMAL,
        GT._("Save"), false);
    buttonSaveAutomaticFixing.setActionCommand(ACTION_SAVE_AUTOMATIC_FIXING);
    buttonSaveAutomaticFixing.addActionListener(this);
    toolBarButtons.add(buttonSaveAutomaticFixing);
    buttonRunAutomaticFixing = Utilities.createJButton(
        "gnome-system-run.png", EnumImageSize.NORMAL,
        GT._("Fix selected pages"), false);
    buttonRunAutomaticFixing.setActionCommand(ACTION_RUN_AUTOMATIC_FIXING);
    buttonRunAutomaticFixing.addActionListener(this);
    toolBarButtons.add(buttonRunAutomaticFixing);
    constraints.fill = GridBagConstraints.VERTICAL;
    constraints.gridy = 0;
    panel.add(toolBarButtons, constraints);
    constraints.gridx++;

    // Automatic fixing list
    modelAutomaticFixing = new DefaultListModel();
    listAutomaticFixing = new JList(modelAutomaticFixing);
    listAutomaticFixing.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
    JScrollPane scrollAutomaticFixing = new JScrollPane(listAutomaticFixing);
    scrollAutomaticFixing.setMinimumSize(new Dimension(100, 100));
    scrollAutomaticFixing.setPreferredSize(new Dimension(200, 150));
    scrollAutomaticFixing.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_ALWAYS);
    constraints.fill = GridBagConstraints.BOTH;
    constraints.gridy = 0;
    constraints.weightx = 1;
    panel.add(scrollAutomaticFixing, constraints);
    constraints.gridx++;

    return panel;
  }

  /**
   * @return Links components.
   */
  private Component createLinksComponents() {
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
    constraints.weightx = 1;
    constraints.weighty = 0;

    // Select next links button
    buttonSelectNextLinks = Utilities.createJButton(GT._("Select &next links"));
    buttonSelectNextLinks.setActionCommand(ACTION_NEXT_LINKS);
    buttonSelectNextLinks.addActionListener(this);
    constraints.fill = GridBagConstraints.HORIZONTAL;
    constraints.weightx = 1;
    panel.add(buttonSelectNextLinks, constraints);
    constraints.gridy++;

    // Button toolbar
    JToolBar toolbar = new JToolBar(SwingConstants.HORIZONTAL);
    toolbar.setFloatable(false);
    buttonFullAnalysisLink = Utilities.createJButton(
        "gnome-system-run.png", EnumImageSize.NORMAL,
        GT._("Full analysis (Alt + &F)"), false);
    buttonFullAnalysisLink.setActionCommand(ACTION_FULL_ANALYSIS_LINK);
    buttonFullAnalysisLink.addActionListener(this);
    toolbar.add(buttonFullAnalysisLink);
    buttonDisambiguationLink = Utilities.createJButton(
        "commons-disambig-colour.png", EnumImageSize.NORMAL,
        GT._("Disambiguation (Alt + &D)"), false);
    buttonDisambiguationLink.setActionCommand(ACTION_DISAMBIGUATION_LINK);
    buttonDisambiguationLink.addActionListener(this);
    toolbar.add(buttonDisambiguationLink);
    buttonExternalViewerLink = Utilities.createJButton(
        "gnome-emblem-web.png", EnumImageSize.NORMAL,
        GT._("External Viewer"), false);
    buttonExternalViewerLink.setActionCommand(ACTION_EXTERNAL_VIEWER_LINK);
    buttonExternalViewerLink.addActionListener(this);
    toolbar.add(buttonExternalViewerLink);
    toolbar.addSeparator();
    linkCount = new JLabel(GT._("Link count"));
    toolbar.add(linkCount);
    constraints.fill = GridBagConstraints.HORIZONTAL;
    constraints.weightx = 1;
    constraints.weighty = 0;
    panel.add(toolbar, constraints);
    constraints.gridy++;

    // Links
    listLinks = new JList(modelLinks);
    listLinks.setCellRenderer(new PageListCellRenderer());
    listLinks.addMouseListener(new PageListPopupListener(getWikipedia(), getTextContents(), this));
    listLinks.addMouseListener(new PageListAnalyzeListener(getWikipedia(), this));
    JScrollPane scrollLinks = new JScrollPane(listLinks);
    scrollLinks.setMinimumSize(new Dimension(100, 100));
    scrollLinks.setPreferredSize(new Dimension(200, 500));
    scrollLinks.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_ALWAYS);
    constraints.fill = GridBagConstraints.BOTH;
    constraints.weighty = 1;
    panel.add(scrollLinks, constraints);
    constraints.gridy++;

    return panel;
  }

  /**
   * @return Sort menu.
   */
  private JMenu createSortMenu() {
    JMenu menu = Utilities.createJMenu(GT._("Sort"));
    List<CompositeComparator<Page>> comparators = PageComparator.getComparators();
    for (CompositeComparator<Page> comparator : comparators) {
      JMenuItem menuItem = Utilities.createJMenuItem(comparator.getName());
      menuItem.addActionListener(new SetComparatorAction(modelLinks, comparator));
      menu.add(menuItem);
    }
    return menu;
  }

  /* (non-Javadoc)
   * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
   */
  @Override
  public void actionPerformed(ActionEvent e) {
    if (e == null) {
      return;
    }

    super.actionPerformed(e);
    if (ACTION_FULL_ANALYSIS_LINK.equals(e.getActionCommand())) {
      actionFullAnalysisLink();
    } else if (ACTION_DISAMBIGUATION_LINK.equals(e.getActionCommand())) {
      actionDisambiguationLink();
    } else if (ACTION_EXTERNAL_VIEWER_LINK.equals(e.getActionCommand())) {
      actionExternalViewerLink();
    } else if (ACTION_NEXT_LINKS.equals(e.getActionCommand())) {
      actionSelectNextLinks();
    } else if (ACTION_ADD_AUTOMATIC_FIXING.equals(e.getActionCommand())) {
      actionAddAutomaticFixing();
    } else if (ACTION_MDF_AUTOMATIC_FIXING.equals(e.getActionCommand())) {
      actionMdfAutomaticFixing();
    } else if (ACTION_RMV_AUTOMATIC_FIXING.equals(e.getActionCommand())) {
      ActionRmvAutomaticFixing();
    } else if (ACTION_CLR_AUTOMATIC_FIXING.equals(e.getActionCommand())) {
      actionClrAutomaticFixing();
    } else if (ACTION_RUN_AUTOMATIC_FIXING.equals(e.getActionCommand())) {
      actionRunAutomaticFixing();
    } else if (ACTION_SAVE_AUTOMATIC_FIXING.equals(e.getActionCommand())) {
      actionSaveAutomaticFixing();
    }
  }

  /**
   * Clean page. 
   */
  @Override
  protected void clean() {
    super.clean();
    modelLinks.clear();
    updateComponentState();
  }

  /**
   * Action called when Reload button is pressed. 
   */
  @Override
  protected void actionReload() {
    clean();
    DisambiguationAnalysisWorker reloadWorker = new DisambiguationAnalysisWorker(
        getWikipedia(), this, getPage());
    setupReloadWorker(reloadWorker);
    reloadWorker.start();
  }

  /**
   * Callback called at the end of the Reload Worker.
   */
  @Override
  protected void afterFinishedReloadWorker() {
    super.afterFinishedReloadWorker();
    Page page = getPage();
    List<Page> links = page.getBackLinksWithRedirects();
    modelLinks.setElements(links);
    Integer countMain = page.getBacklinksCountInMainNamespace();
    Integer countTotal = page.getBacklinksCount();
    linkCount.setText(
        ((countMain != null) ? countMain.toString() : "?") +
        " / " +
        ((countTotal != null) ? countTotal.toString() : "?"));

    // Update automatic fixing
    Configuration config = Configuration.getConfiguration();
    Object[] automaticFixing = config.getPojoArray(
        Configuration.POJO_AUTOMATIC_FIXING, page.getTitle(), AutomaticFixing.class);
    if (automaticFixing != null) {
      modelAutomaticFixing.clear();
      for (int i = 0; i < automaticFixing.length; i++) {
        modelAutomaticFixing.addElement(automaticFixing[i]);
      }
    }
  }

  /**
   * Callback called before the start of the Reload Worker. 
   */
  @Override
  protected void beforeStartReloadWorker() {
    super.beforeStartReloadWorker();
    modelLinks.setShowDisambiguation(true);
    modelLinks.setShowOther(true);
  }

  /**
   * Action called when Add Automatic Fixing button is pressed.
   */
  private void actionAddAutomaticFixing() {
    String initialText = Utilities.askForValue(
        getParentComponent(), "Input the text that should be replaced", "", null);
    if ((initialText == null) || (initialText.length() == 0)) {
      return;
    }
    String replaceText = Utilities.askForValue(
        getParentComponent(), "Input the text that should be used as replacement", "", null);
    if ((replaceText == null) || (replaceText.length() == 0)) {
      return;
    }
    modelAutomaticFixing.addElement(new AutomaticFixing(initialText, replaceText));
  }

  /**
   * Action called when Modify Automatic Fixing button is pressed. 
   */
  private void actionMdfAutomaticFixing() {
    Object selected = listAutomaticFixing.getSelectedValue();
    if (selected instanceof AutomaticFixing) {
      AutomaticFixing fixing = (AutomaticFixing) selected;
      String initialText = Utilities.askForValue(
          getParentComponent(), "Input the text that should be replaced",
          fixing.getOriginalText(), null);
      if ((initialText == null) || (initialText.length() == 0)) {
        return;
      }
      fixing.setOriginalText(initialText);
      String replaceText = Utilities.askForValue(
          getParentComponent(), "Input the text that should be used as replacement",
          fixing.getReplacementText(), null);
      if ((replaceText == null) || (replaceText.length() == 0)) {
        return;
      }
      fixing.setReplacementText(replaceText);
      listAutomaticFixing.repaint();
    }
  }

  /**
   * Action called when Remove Automatic Fixing button is pressed.
   */
  private void ActionRmvAutomaticFixing() {
    int selected = listAutomaticFixing.getSelectedIndex();
    if (selected != - 1) {
      modelAutomaticFixing.remove(selected);
    }
  }

  /**
   * Action called when Clear Automatic Fixing button is pressed. 
   */
  private void actionClrAutomaticFixing() {
    modelAutomaticFixing.clear();
  }

  /**
   * Action called when Run Automatic Fixing button is pressed. 
   */
  private void actionRunAutomaticFixing() {
    Object[] values = listLinks.getSelectedValues();
    if ((values == null) || (values.length == 0)) {
      Utilities.displayWarning(
          getParentComponent(),
          GT._("You must select the pages on which running automatic disambiguation."));
      return;
    }
    if (modelAutomaticFixing.isEmpty()) {
      Utilities.displayWarning(
          getParentComponent(),
          GT._("You must input the initial and destination texts."));
      return;
    }
    int answer = Utilities.displayYesNoWarning(
        getParentComponent(),
        GT._("!!! WARNING !!!") + "\n" +
        GT._("This function will do all the replacements in all selected pages.") + "\n" +
        GT._("It may modify a lot of pages in a short period of time.") + "\n" +
        GT._("On some Wikipedia projects, you may need the bot status for doing this.") + "\n" +
        GT._("Please, check if you need the bot status by reading the rules of Wikipedia.") + "\n" +
        GT._("Also, verify again the texts you have inputed before running this function.") + "\n" +
        GT._("Do you want to continue ?"));
    if (answer != JOptionPane.YES_OPTION) {
      return;
    }
    Page[] pages = new Page[values.length];
    for (int i = 0; i < values.length; i++) {
      pages[i] = (Page) values[i];
    }
    Properties replacement = new Properties();
    for (int i = 0; i < modelAutomaticFixing.getSize(); i++) {
      Object value = modelAutomaticFixing.get(i);
      if (value instanceof AutomaticFixing) {
        AutomaticFixing replacementValue = (AutomaticFixing) value;
        replacement.setProperty(
            replacementValue.getOriginalText(),
            replacementValue.getReplacementText());
      }
    }
    Map<String, Properties> replacements = new HashMap<String, Properties>();
    replacements.put("[[" + getPage().getTitle() + "]]", replacement);
    AutomaticDisambiguationWorker dabWorker = new AutomaticDisambiguationWorker(
        getWikipedia(), this, pages, replacements,
        getWikipedia().getUpdatePageMessage(),
        true);
    dabWorker.setListener(new DefaultBasicWorkerListener() {
      @Override
      public void afterFinished(
          BasicWorker worker,
          @SuppressWarnings("unused") boolean ok) {
        if (!worker.shouldContinue()) {
          return;
        }
        if (worker.get() instanceof Integer) {
          Integer count = (Integer) worker.get();
          if (count.intValue() == 0) {
            return;
          }
        }
        actionReload();
      }
    });
    dabWorker.start();
  }

  /**
   * Action called when Save Automatic Fixing button is pressed. 
   */
  private void actionSaveAutomaticFixing() {
    Configuration config = Configuration.getConfiguration();
    Object[] replacements = modelAutomaticFixing.toArray();
    Arrays.sort(replacements, new Comparator<Object>() {

      public int compare(Object o1, Object o2) {
        AutomaticFixing a1 = (o1 instanceof AutomaticFixing) ? (AutomaticFixing) o1 : null;
        AutomaticFixing a2 = (o2 instanceof AutomaticFixing) ? (AutomaticFixing) o2 : null;
        if (a2 == null) {
          if (a1 == null) {
            return 0;
          }
          return -1;
        }
        if (a1 == null) {
          return 1;
        }
        return a1.getOriginalText().compareTo(a2.getOriginalText());
      }
      
    });
    config.addPojoArray(Configuration.POJO_AUTOMATIC_FIXING, replacements, getPage().getTitle());
  }

  /**
   * Action called when Full analysis button is pressed.
   */
  private void actionFullAnalysisLink() {
    List<Page> knownPages = null;
    if (getPage() != null) {
      knownPages = new ArrayList<Page>(1);
      knownPages.add(getPage());
      for (Page backLink : getPage().getBackLinksWithRedirects()) {
        if ((backLink != null) &&
            (backLink.isRedirect()) &&
            (Page.areSameTitle(getPage().getTitle(), backLink.getRedirectDestination()))) {
          knownPages.add(backLink);
        }
      }
    }
    Controller.runFullAnalysis(
        getParentComponent(), listLinks.getSelectedValues(), knownPages, getWikipedia());
  }

  /**
   * Action called when Disambiguation button is pressed.
   */
  private void actionDisambiguationLink() {
    Controller.runDisambiguationAnalysis(
        getParentComponent(), listLinks.getSelectedValues(), getWikipedia());
  }

  /**
   * Action called when External Viewer button is pressed.
   */
  private void actionExternalViewerLink() {
    for (Object selection : listLinks.getSelectedValues()) {
      if (selection instanceof Page) {
        Utilities.browseURL(getWikipedia(), ((Page) selection).getTitle(), false);
      }
    }
  }

  /**
   * Action called when Select next links button is pressed. 
   */
  private void actionSelectNextLinks() {
    int last = listLinks.getMaxSelectionIndex() + 1;
    Configuration config = Configuration.getConfiguration();
    int count = Math.min(
        listLinks.getModel().getSize() - last,
        config.getInt(
            Configuration.INTEGER_MAXIMUM_PAGES,
            Configuration.DEFAULT_MAXIMUM_PAGES));
    int indices[] = new int[count];
    for (int i = 0; i < count; i++) {
      indices[i] = last + i;
    }
    listLinks.setSelectedIndices(indices);
    if (count > 0) {
      listLinks.ensureIndexIsVisible(indices[count - 1]);
    } else {
      listLinks.ensureIndexIsVisible(0);
    }
  }
}

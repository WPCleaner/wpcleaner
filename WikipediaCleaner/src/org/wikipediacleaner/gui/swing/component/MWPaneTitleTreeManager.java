/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.gui.swing.component;

import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionListener;
import java.beans.EventHandler;
import java.util.List;

import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JSplitPane;
import javax.swing.JToolBar;
import javax.swing.JTree;
import javax.swing.ScrollPaneConstants;
import javax.swing.SwingConstants;
import javax.swing.event.TreeSelectionListener;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.DefaultTreeCellRenderer;
import javax.swing.tree.DefaultTreeModel;
import javax.swing.tree.TreeNode;
import javax.swing.tree.TreePath;
import javax.swing.tree.TreeSelectionModel;

import org.wikipediacleaner.api.data.PageAnalysis;
import org.wikipediacleaner.api.data.PageElementTitle;
import org.wikipediacleaner.gui.swing.basic.Utilities;
import org.wikipediacleaner.i18n.GT;
import org.wikipediacleaner.images.EnumImageSize;


/**
 * A manager for the tree of titles.
 */
public class MWPaneTitleTreeManager {

  private final MWPane textPane;
  private final JSplitPane splitPane;
  private final JTree treeToc;
  private final DefaultTreeModel modelToc;
  private final JTree treeToc2;
  private final DefaultTreeModel modelToc2;

  private boolean tocIsDisplayed;

  /**
   * Create the title tree manager.
   * 
   * @param textPane Text pane.
   */
  public MWPaneTitleTreeManager(MWPane textPane) {

    // Text pane
    this.textPane = textPane;
    splitPane = new JSplitPane(JSplitPane.VERTICAL_SPLIT);
    JScrollPane scrollContents = new JScrollPane(textPane);
    scrollContents.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_ALWAYS);
    splitPane.setBottomComponent(scrollContents);

    // Table of contents
    JPanel panelTOC = new JPanel(new GridBagLayout());
    GridBagConstraints constraints = new GridBagConstraints();
    constraints.fill = GridBagConstraints.BOTH;
    constraints.gridheight = 1;
    constraints.gridwidth = 1;
    constraints.gridx = 0;
    constraints.gridy = 0;
    constraints.insets = new Insets(0, 0, 0, 0);
    constraints.ipadx = 0;
    constraints.ipady = 0;
    constraints.weightx = 1;
    constraints.weighty = 1;

    // Toolbar
    JToolBar toolbarButtons = new JToolBar(SwingConstants.VERTICAL);
    toolbarButtons.setFloatable(false);
    JButton buttonLess = Utilities.createJButton(
        "gnome-go-previous.png", EnumImageSize.NORMAL,
        GT._T("Decrement title level"), false, null);
    buttonLess.addActionListener(EventHandler.create(
        ActionListener.class, this, "decreaseLevel"));
    toolbarButtons.add(buttonLess);
    JButton buttonMore = Utilities.createJButton(
        "gnome-go-next.png", EnumImageSize.NORMAL,
        GT._T("Increment title level"), false, null);
    buttonMore.addActionListener(EventHandler.create(
        ActionListener.class, this, "increaseLevel"));
    toolbarButtons.add(buttonMore);
    JButton buttonDone = Utilities.createJButton(
        "commons-approve-icon.png", EnumImageSize.NORMAL,
        GT._T("Validate the new table of contents"), false, null);
    buttonDone.addActionListener(EventHandler.create(
        ActionListener.class, this, "validate"));
    toolbarButtons.add(buttonDone);
    constraints.weightx = 0;
    panelTOC.add(toolbarButtons, constraints);
    constraints.gridx++;

    // Tree node renderer
    DefaultTreeCellRenderer rendererToc = new DefaultTreeCellRenderer();
    rendererToc.setLeafIcon(rendererToc.getClosedIcon());

    // Table of contents Tree
    MWPaneTitleTreeNode rootToc = new MWPaneTitleTreeNode(null);
    modelToc = new DefaultTreeModel(rootToc);
    treeToc = new JTree(modelToc);
    treeToc.setRootVisible(false);
    treeToc.setShowsRootHandles(true);
    treeToc.getSelectionModel().setSelectionMode(
        TreeSelectionModel.CONTIGUOUS_TREE_SELECTION);
    treeToc.setCellRenderer(rendererToc);
    treeToc.addTreeSelectionListener(EventHandler.create(
        TreeSelectionListener.class, this, "selectionChanged"));
    JScrollPane scrollTree = new JScrollPane(treeToc);
    scrollTree.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_ALWAYS);
    constraints.weightx = 1;
    panelTOC.add(scrollTree, constraints);
    constraints.gridx++;

    // Second tree
    MWPaneTitleTreeNode rootToc2 = new MWPaneTitleTreeNode(null);
    modelToc2 = new DefaultTreeModel(rootToc2);
    treeToc2 = new JTree(modelToc2);
    treeToc2.setRootVisible(false);
    treeToc2.setShowsRootHandles(true);
    treeToc2.setCellRenderer(rendererToc);
    JScrollPane scrollTree2 = new JScrollPane(treeToc2);
    scrollTree2.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_ALWAYS);
    constraints.weightx = 1;
    panelTOC.add(scrollTree2, constraints);
    constraints.gridx++;
    
    splitPane.setTopComponent(panelTOC);

    // Hide table of contents
    hideToc();
  }

  /**
   * @return Component containing the trees.
   */
  public JComponent getComponent() {
    return splitPane;
  }

  /**
   * Toggle display of table of contents.
   */
  public void toggleToc() {
    if (tocIsDisplayed) {
      hideToc();
    } else {
      displayToc(null);
    }
  }

  /**
   * Display table of contents.
   * 
   * @param title Title to be selected.
   */
  public void displayToc(PageElementTitle title) {
    if (!tocIsDisplayed) {
      updateTreeToc();
      updateTreeToc2();
      selectTreeToc2();
      tocIsDisplayed = true;
    }
    splitPane.setDividerLocation(200);
    splitPane.setDividerSize(2);
    splitPane.setResizeWeight(0.0);
    textPane.setEditableInternal(false);
    if (title != null) {
      MWPaneTitleTreeNode node = findTreeNode(
          (MWPaneTitleTreeNode) modelToc.getRoot(),
          title);
      if (node != null) {
        TreePath treePath = new TreePath(node.getPath());
        treeToc.scrollPathToVisible(treePath);
        treeToc.setSelectionPath(treePath);
      }
    }
  }

  /**
   * Hide table of contents.
   */
  public void hideToc() {
    tocIsDisplayed = false;
    splitPane.setDividerLocation(0);
    splitPane.setDividerSize(0);
    splitPane.setResizeWeight(0.0);
    textPane.setEditableInternal(textPane.isEditable);
  }

  /**
   * Apply change in selection.
   */
  public void selectionChanged() {
    MWPaneTitleTreeNode treeNode = (MWPaneTitleTreeNode) treeToc.getLastSelectedPathComponent();
    if (treeNode == null) {
      return;
    }
    Object nodeInfo = treeNode.getUserObject();
    if (nodeInfo instanceof PageElementTitle) {
      PageElementTitle title = (PageElementTitle) nodeInfo;
      try {
        textPane.setCaretPosition(title.getBeginIndex());
        textPane.moveCaretPosition(title.getEndIndex());
      } catch (IllegalArgumentException e2) {
        //
      }
      textPane.requestFocusInWindow();
    }
    selectTreeToc2();
  }

  /**
   * Validate modifications.
   */
  public void validate() {
    StringBuilder contents = new StringBuilder(textPane.getText());
    applyChanges(contents, treeToc.getModel().getRoot());
    textPane.changeText(contents.toString());
    treeToc.repaint();
    textPane.requestFocusInWindow();
  }

  /**
   * Increase level of the currently selected titles.
   */
  public void increaseLevel() {
    changeLevel(1);
  }

  /**
   * Decrease level of the currently selected titles.
   */
  public void decreaseLevel() {
    changeLevel(-1);
  }

  /**
   * Change level of the currently selected titles.
   * 
   * @param delta Delta to apply.
   */
  private void changeLevel(int delta) {
    if (treeToc == null) {
      return;
    }
    TreePath[] selections = treeToc.getSelectionPaths();
    if (selections == null) {
      return;
    }
    for (int i = 0; i < selections.length; i++) {
      TreePath selection = selections[i];
      boolean use = true;
      for (int j = 0; j < i; j++) {
        if (selections[j].isDescendant(selection)) {
          use = false;
        }
      }
      if (use) {
        MWPaneTitleTreeNode treeNode = (MWPaneTitleTreeNode) selection.getLastPathComponent();
        changeTitleLevel(treeNode, delta);
      }
    }
    updateTreeToc2();
    selectTreeToc2();
    treeToc.repaint();
    textPane.requestFocusInWindow();
  }

  /**
   * Update table of contents tree.
   */
  private void updateTreeToc() {
    MWPaneTitleTreeNode rootNode = new MWPaneTitleTreeNode(null);
    MWPaneTitleTreeNode lastNode = rootNode;
    PageAnalysis pageAnalysis = textPane.getWikiPage().getAnalysis(textPane.getText(), true);
    List<PageElementTitle> titles = pageAnalysis.getTitles();
    for (PageElementTitle title : titles) {
      while ((lastNode != null) &&
             (lastNode.getInitialTitleLevel() >= title.getLevel())) {
        if (lastNode.getParent() != null) {
          lastNode = (MWPaneTitleTreeNode) lastNode.getParent();
        } else {
          lastNode = null;
        }
      }
      if (lastNode == null) {
        lastNode = rootNode;
      }
      MWPaneTitleTreeNode tmpNode = new MWPaneTitleTreeNode(title);
      lastNode.add(tmpNode);
      lastNode = tmpNode;
    }
    modelToc.setRoot(rootNode);
  }

  /**
   * Update table of contents second tree. 
   */
  private void updateTreeToc2() {
    MWPaneTitleTreeNode rootNode2 = new MWPaneTitleTreeNode(null);
    MWPaneTitleTreeNode rootNode = (MWPaneTitleTreeNode) modelToc.getRoot();
    updateTreeToc2Node(rootNode2, rootNode);
    modelToc2.setRoot(rootNode2);
  }

  /**
   * Select node in the second tree depending on the selection in the first tree.
   */
  private void selectTreeToc2() {
    MWPaneTitleTreeNode treeNode = (MWPaneTitleTreeNode) treeToc.getLastSelectedPathComponent();
    if (treeNode == null) {
      treeToc2.setSelectionRows(null);
    } else {
      MWPaneTitleTreeNode otherNode = findTreeNode(
          (MWPaneTitleTreeNode) modelToc2.getRoot(),
          treeNode.getTitle());
      if (otherNode != null) {
        TreePath treePath = new TreePath(otherNode.getPath());
        treeToc2.scrollPathToVisible(treePath);
        treeToc2.setSelectionPath(treePath);
      }
    }
  }

  /**
   * Find a tree node matching a title.
   * 
   * @param node Current node.
   * @param title Title.
   * @return Node matching the title if found.
   */
  private MWPaneTitleTreeNode findTreeNode(
      MWPaneTitleTreeNode node, PageElementTitle title) {
    if (node == null) {
      return null;
    }
    if (title == node.getTitle()) {
      return node;
    }
    for (int i = 0; i < node.getChildCount(); i++) {
      MWPaneTitleTreeNode found = findTreeNode(
          (MWPaneTitleTreeNode) node.getChildAt(i), title);
      if (found != null) {
        return found;
      }
    }
    return null;
  }

  /**
   * Update table of contents second tree for a node and its children.
   * 
   * @param rootNode2 Root of second tree.
   * @param node Current node to add.
   */
  private void updateTreeToc2Node(
      MWPaneTitleTreeNode rootNode2,
      MWPaneTitleTreeNode node) {
    if (node == null) {
      return;
    }
    for (int i = 0; i < node.getChildCount(); i++) {
      MWPaneTitleTreeNode currentNode = (MWPaneTitleTreeNode) node.getChildAt(i);
      MWPaneTitleTreeNode newNode = new MWPaneTitleTreeNode(currentNode.getTitle());
      newNode.setCurrentTitleLevel(currentNode.getCurrentTitleLevel());
      MWPaneTitleTreeNode parentNode = (MWPaneTitleTreeNode) rootNode2.getLastLeaf();
      while ((parentNode.isRoot() == false) &&
             (parentNode.getCurrentTitleLevel() >= newNode.getCurrentTitleLevel())) {
        parentNode = (MWPaneTitleTreeNode) parentNode.getParent();
      }
      parentNode.add(newNode);
      updateTreeToc2Node(rootNode2, currentNode);
    }
  }

  /**
   * Change title level (including children).
   * 
   * @param treeNode Node.
   * @param increment Increment.
   */
  private void changeTitleLevel(MWPaneTitleTreeNode treeNode, int increment) {
    if (treeNode.getCurrentTitleLevel() + increment > 0) {
      treeNode.setCurrentTitleLevel(treeNode.getCurrentTitleLevel() + increment);
      for (int i = 0; i < treeNode.getChildCount(); i++) {
        TreeNode child = treeNode.getChildAt(i);
        if (child instanceof MWPaneTitleTreeNode) {
          changeTitleLevel((MWPaneTitleTreeNode) treeNode.getChildAt(i), increment);
        }
      }
    }
  }

  /**
   * Save changes to table of contents.
   * 
   * @param contents Contents.
   * @param node Node.
   */
  private void applyChanges(StringBuilder contents, Object node) {
    if ((contents == null) || (node == null)) {
      return;
    }
    if (!(node instanceof MWPaneTitleTreeNode)) {
      return;
    }
    MWPaneTitleTreeNode treeNode = (MWPaneTitleTreeNode) node;
    for (int i = treeNode.getChildCount(); i > 0; i--) {
      applyChanges(contents, treeNode.getChildAt(i - 1));
    }
    if (treeNode.getCurrentTitleLevel() != treeNode.getInitialTitleLevel()) {
      StringBuilder newTitle = new StringBuilder();
      for (int i = 0; i < treeNode.getCurrentTitleLevel(); i++) {
        newTitle.append("=");
      }
      newTitle.append(" ");
      newTitle.append(treeNode.getTitle().getTitle());
      newTitle.append(" ");
      for (int i = 0; i < treeNode.getCurrentTitleLevel(); i++) {
        newTitle.append("=");
      }
      contents.replace(
          treeNode.getTitle().getBeginIndex(),
          treeNode.getTitle().getEndIndex(),
          newTitle.toString());
      textPane.hideToc();
      textPane.resetAttributes();
      textPane.requestFocusInWindow();
    }
  }
}

/**
 * A tree node for titles. 
 */
class MWPaneTitleTreeNode extends DefaultMutableTreeNode {

  private static final long serialVersionUID = 1L;

  private final PageElementTitle title;
  private int level;

  /**
   * @param title Title.
   */
  public MWPaneTitleTreeNode(PageElementTitle title) {
    super((title != null) ? title : "Page");
    this.title = title;
    this.level = (title != null) ? title.getLevel() : 0;
  }

  /**
   * @return Title level.
   */
  public int getInitialTitleLevel() {
    if (title != null) {
      return title.getLevel();
    }
    return 0;
  }

  /**
   * @return Title level.
   */
  public int getCurrentTitleLevel() {
    return level;
  }

  /**
   * @param level Title level.
   */
  public void setCurrentTitleLevel(int level) {
    this.level = level;
  }

  /**
   * @return Title.
   */
  public PageElementTitle getTitle() {
    return title;
  }

  /* (non-Javadoc)
   * @see javax.swing.tree.DefaultMutableTreeNode#toString()
   */
  @Override
  public String toString() {
    if (title == null) {
      return super.toString();
    }
    StringBuilder buffer = new StringBuilder();
    buffer.append("(");
    buffer.append(title.getLevel() - 1);
    if (title.getLevel() != level) {
      buffer.append(" -> ");
      buffer.append(level - 1);
    }
    buffer.append(") ");
    buffer.append(title.getTitle());
    return buffer.toString();
  }
}

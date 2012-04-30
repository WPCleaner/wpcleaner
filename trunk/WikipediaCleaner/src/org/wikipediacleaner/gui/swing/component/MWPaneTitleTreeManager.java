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

package org.wikipediacleaner.gui.swing.component;

import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
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
import javax.swing.event.TreeSelectionEvent;
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
public class MWPaneTitleTreeManager
  implements TreeSelectionListener, ActionListener {

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
        GT._("Decrement title level"), false);
    buttonLess.setActionCommand("-");
    buttonLess.addActionListener(this);
    toolbarButtons.add(buttonLess);
    JButton buttonMore = Utilities.createJButton(
        "gnome-go-next.png", EnumImageSize.NORMAL,
        GT._("Increment title level"), false);
    buttonMore.setActionCommand("+");
    buttonMore.addActionListener(this);
    toolbarButtons.add(buttonMore);
    JButton buttonDone = Utilities.createJButton(
        "commons-approve-icon.png", EnumImageSize.NORMAL,
        GT._("Validate the new table of contents"), false);
    buttonDone.setActionCommand("OK");
    buttonDone.addActionListener(this);
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
        TreeSelectionModel.SINGLE_TREE_SELECTION);
    treeToc.setCellRenderer(rendererToc);
    treeToc.addTreeSelectionListener(this);
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
      displayToc();
    }
  }

  /**
   * Dispay table of contents.
   */
  public void displayToc() {
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

  /* (non-Javadoc)
   * @see javax.swing.event.TreeSelectionListener#valueChanged(javax.swing.event.TreeSelectionEvent)
   */
  public void valueChanged(@SuppressWarnings("unused") TreeSelectionEvent e) {
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

  /* (non-Javadoc)
   * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
   */
  public void actionPerformed(ActionEvent e) {
    if (treeToc == null) {
      return;
    }
    MWPaneTitleTreeNode treeNode = (MWPaneTitleTreeNode) treeToc.getLastSelectedPathComponent();
    if (treeNode == null) {
      return;
    }
    if ("+".equals(e.getActionCommand())) {
      changeTitleLevel(treeNode, 1);
      updateTreeToc2();
      selectTreeToc2();
    } else if ("-".equals(e.getActionCommand())) {
      changeTitleLevel(treeNode, -1);
      updateTreeToc2();
      selectTreeToc2();
    } else if ("OK".equals(e.getActionCommand())) {
      StringBuilder contents = new StringBuilder(textPane.getText());
      applyChanges(contents, treeToc.getModel().getRoot());
      textPane.changeText(contents.toString());
    }
    treeToc.repaint();
    textPane.requestFocusInWindow();
  }

  /**
   * Update table of contents tree.
   */
  private void updateTreeToc() {
    MWPaneTitleTreeNode rootNode = new MWPaneTitleTreeNode(null);
    MWPaneTitleTreeNode lastNode = rootNode;
    PageAnalysis pageAnalysis = new PageAnalysis(textPane.getWikiPage(), textPane.getText());
    List<PageElementTitle> titles = pageAnalysis.getTitles();
    for (PageElementTitle title : titles) {
      while ((lastNode != null) &&
             (lastNode.getInitialTitleLevel() >= title.getFirstLevel())) {
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
    this.level = (title != null) ? title.getFirstLevel() : 0;
  }

  /**
   * @return Title level.
   */
  public int getInitialTitleLevel() {
    if (title != null) {
      return title.getFirstLevel();
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
    buffer.append(title.getFirstLevel() - 1);
    if (title.getFirstLevel() != level) {
      buffer.append(" -> ");
      buffer.append(level - 1);
    }
    buffer.append(") ");
    buffer.append(title.getTitle());
    return buffer.toString();
  }
}

package org.openapitools.codegen.utils;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.HashSet;

public class TreeNode<T> {
    
    private T data;
    private List<TreeNode<T>> children;
    private List<TreeNode<T>> parents;
    
    public TreeNode(T data) {
		this.data = data;
        this.children = new ArrayList<TreeNode<T>>();
        this.parents = new ArrayList<TreeNode<T>>();
    }
    
    public TreeNode<T> addChild(T child) {
        TreeNode<T> node = new TreeNode<T>(child);
        addChild(node);
        return node;
    }

    public void addChild(TreeNode<T> childNode) {
        this.children.add(childNode);
        childNode.parents.add(this);
    }

    public T getData() {
        return data;
    }

    private void getAncestors(Set<T> ancestors) {
        for (TreeNode<T> parent: parents) {
            if (!ancestors.add(parent.data)) {
                // The ancestor already exists in the set. There is a loop.
                throw new RuntimeException("Cyclic loop in ancestor hierarchy");
            }
            parent.getAncestors(ancestors);
        }
    }

    public Set<T> getAncestors() {
        Set<T> ancestors = new HashSet<T>();
        getAncestors(ancestors);
        return ancestors;
    }

    private void getDescendants(Set<T> descendants) {
        for (TreeNode<T> child: children) {
            if (!descendants.add(child.data)) {
                // The descendant already exists in the set. There is a loop.
                throw new RuntimeException("Cyclic loop in descendant hierarchy");
            }
            child.getDescendants(descendants);
        }
    }

    public Set<T> getDescendants() {
        Set<T> descendants = new HashSet<T>();
        getDescendants(descendants);
        return descendants;
    }
}
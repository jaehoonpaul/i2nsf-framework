import numpy as np

# declare class for node of decision tree
class DecisionTreeNode:
    def __init__(self, learning_input, learning_output, generalize_rate, capabilitylist):
        self.learning_input = learning_input
        self.learning_output = learning_output
        self.generalize_rate = generalize_rate
        self.capabilitylist = capabilitylist
        self.isTerminal = False
        self.separator = ''
        # construct tree
        self.expandNode()

    # calculate entropy of data list
    def selfEntropy(self):
        learningcount = len(self.learning_output)
        truecount = 0
        for i in range(learningcount):
            if self.learning_output[i] == True:
                truecount += 1
        truerate = float(truecount)/learningcount
        falserate = 1.0 - truerate
        entropy = 0.0
        if truerate != 0.0:
            entropy += (-1)*truerate*np.log2(truerate)
        if falserate != 0.0:
            entropy += (-1)*falserate*np.log2(falserate)
        return [entropy, truerate>=falserate]

    def partition_entropy(self, separator_index):
        # declare variable
        learningcount = len(self.learning_output)
        positivecount = negativecount = 0
        positivelist_input = []
        positivelist_output = []
        negativelist_input = []
        negativelist_output = []
        entropy1 = entropy2 = 0.0

        # divide partition
        for i in range(learningcount):
            if self.learning_input[i][separator_index] == True:
                positivelist_input.append(self.learning_input[i])
                positivelist_output.append(self.learning_output[i])
                positivecount += 1
            else:
                negativelist_input.append(self.learning_input[i])
                negativelist_output.append(self.learning_output[i])
                negativecount += 1

        if positivecount == 0 or negativecount == 0:
            return [1.5, positivelist_input, positivelist_output, negativelist_input, negativelist_output]

        # calculate entropy for partition 1
        truecount = 0
        for i in range(positivecount):
            if positivelist_output[i] == True:
                truecount += 1
        truerate = float(truecount)/positivecount
        falserate = 1.0 - truerate
        if truerate != 0.0:
            entropy1 += (-1)*truerate*np.log2(truerate)
        if falserate != 0.0:
            entropy1 += (-1)*falserate*np.log2(falserate)

        # calculate entropy for partition 2
        truecount = 0
        for i in range(negativecount):
            if negativelist_output[i] == True:
                truecount += 1
        truerate = float(truecount)/negativecount
        falserate = 1.0 - truerate
        if truerate != 0.0:
            entropy2 += (-1)*truerate*np.log2(truerate)
        if falserate != 0.0:
            entropy2 += (-1)*falserate*np.log2(falserate)

        # calculate average of entropy
        positiverate = float(positivecount)/learningcount
        negativerate = 1.0 - positiverate
        average_entropy = positiverate*entropy1+negativerate*entropy2

        # return useful values
        return [average_entropy, positivelist_input, positivelist_output, negativelist_input, negativelist_output]

    # expanding node by recursive method
    def expandNode(self):
        # calculate entropy of data list
        entropylist = self.selfEntropy()

        # if entropy is pretty low: generalization
        if entropylist[0] <= self.generalize_rate:
            self.isTerminal = True
            self.result = entropylist[1]
        # else: expand tree
        else:
            minentropy = 10.0
            positivelist_input = []
            positivelist_output = []
            negativelist_input = []
            negativelist_output = []
            for i in range(len(self.capabilitylist)):
                partitioning_result = self.partition_entropy(i)
                if partitioning_result[0] < minentropy:
                    minentropy = partitioning_result[0]
                    positivelist_input = partitioning_result[1]
                    positivelist_output = partitioning_result[2]
                    negativelist_input = partitioning_result[3]
                    negativelist_output = partitioning_result[4]
                    self.separator = self.capabilitylist[i]
            if len(positivelist_input) == 0 or len(negativelist_input) == 0:
                self.isTerminal = True
                self.result = entropylist[1]
                self.separator = ''
            else:
                self.positive_partition = DecisionTreeNode(positivelist_input, positivelist_output, self.generalize_rate, self.capabilitylist)
                self.negative_partition = DecisionTreeNode(negativelist_input, negativelist_output, self.generalize_rate, self.capabilitylist)

    def running(self, test_input):
        if self.isTerminal == True:
            print('arrrived to terminal: '+str(self.result))
            return self.result
        else:
            for i in range(len(self.capabilitylist)):
                if self.separator == self.capabilitylist[i]:
                    break
            print('branch: '+self.separator)
            if test_input[i] == True:
                return self.positive_partition.running(test_input)
            else:
                return self.negative_partition.running(test_input)

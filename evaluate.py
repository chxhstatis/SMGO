# test the accuracy on patch
# the accuracy of each epoch(0-80)

import tensorflow as tf
from configuration import model_index
from prepare_data import generate_datasets
from train import get_model, process_features
import os

os.environ['CUDA_VISIBLE_DEVICES'] = '2'

labelname = 'cluster'
if __name__ == '__main__':
    # GPU settings
    gpus = tf.config.experimental.list_physical_devices('GPU')
    if gpus:
        for gpu in gpus:
            tf.config.experimental.set_memory_growth(gpu, True)
    for i in range(0, 81, 1):
        save_model_dir = 'MyModel/epoch' + i
        if i == 80:
            save_model_dir = 'MyModel/' + 'model'
        # get the original_dataset
        train_dataset, valid_dataset, test_dataset, train_count, valid_count, test_count = generate_datasets()
        # load the model
        model = get_model()
        model.load_weights(filepath=save_model_dir)
        # model = tf.saved_model.load(save_model_dir)

        # Get the accuracy on the test set
        loss_object = tf.keras.metrics.SparseCategoricalCrossentropy()
        test_loss = tf.keras.metrics.Mean()
        test_accuracy = tf.keras.metrics.SparseCategoricalAccuracy()


        # @tf.function
        def test_step(images, labels):
            predictions = model(images, training=False)
            t_loss = loss_object(labels, predictions)
            test_loss(t_loss)
            test_accuracy(labels, predictions)


        for features in test_dataset:
            test_images, test_labels = process_features(features, data_augmentation=False)
            test_step(test_images, test_labels)
            print("loss: {:.5f}, test accuracy: {:.5f}".format(test_loss.result(),
                                                               test_accuracy.result()))

        print("The accuracy on test set is: {:.3f}%".format(test_accuracy.result() * 100))

